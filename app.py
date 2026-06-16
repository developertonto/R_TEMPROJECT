import os
import shutil
import tempfile
from pathlib import Path

# ── R 환경 설정 (rpy2 임포트 전에 반드시 실행) ─────────────────────────────
def _setup_r_env() -> None:
    """R_HOME·PATH·로케일을 미리 세팅하고 subprocess 인코딩 패치를 적용한다."""
    if not os.environ.get("R_HOME"):
        base = Path("C:/Program Files/R")
        if base.exists():
            candidates = sorted(base.glob("R-*"), reverse=True)
            if candidates:
                os.environ["R_HOME"] = str(candidates[0])

    r_home = os.environ.get("R_HOME", "")
    if r_home:
        r_bin = str(Path(r_home) / "bin" / "x64")
        if r_bin not in os.environ.get("PATH", ""):
            os.environ["PATH"] = r_bin + ";" + os.environ.get("PATH", "")

    os.environ.setdefault("RPY2_CFFI_MODE", "ABI")
    if os.name != 'nt':
        os.environ["LC_ALL"] = "C.UTF-8"
        os.environ["LANG"] = "C.UTF-8"
    else:
        os.environ.pop("LC_ALL", None)
        os.environ.pop("LANG", None)
        os.environ.pop("LANGUAGE", None)
    os.environ.setdefault("R_ENVIRON_USER", "")   # 사용자 .Renviron 무시

    # 한국어 Windows에서 rpy2가 초기화 중 R 출력을 읽을 때
    # 디코딩 실패하는 문제를 패치로 방지한다.
    if os.name == 'nt':
        import subprocess
        _orig_check_output = subprocess.check_output
        
        def _safe_check_output(*args, **kwargs):
            kwargs["errors"] = "replace"
            if "encoding" not in kwargs:
                kwargs["encoding"] = "cp949"
            return _orig_check_output(*args, **kwargs)
            
        subprocess.check_output = _safe_check_output
        
        _orig_run = subprocess.run
        def _safe_run(cmd, *args, **kwargs):
            if kwargs.get("text") and "encoding" not in kwargs:
                kwargs["errors"] = kwargs.get("errors", "replace")
            return _orig_run(cmd, *args, **kwargs)
        subprocess.run = _safe_run

_setup_r_env()
# ────────────────────────────────────────────────────────────────────────────

import pandas as pd
import plotly.express as px
import streamlit as st

import builtins
_orig_open = builtins.open
def _safe_open(file, mode='r', buffering=-1, encoding=None, errors=None, newline=None, closefd=True, opener=None):
    if mode == 'r' and encoding is None:
        errors = 'replace'
    return _orig_open(file, mode, buffering, encoding, errors, newline, closefd, opener)

builtins.open = _safe_open
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter, set_conversion
builtins.open = _orig_open

# Streamlit 스레드 환경에서 rpy2 기본 변환 규칙 유실 방지
set_conversion(ro.default_converter)

# R 코어 분석 스크립트 로드
r_script_path = Path(__file__).parent / "r" / "core_analysis.R"
ro.r(f"source('{r_script_path.as_posix()}', encoding='UTF-8')")
r_process_data = ro.globalenv['process_water_quality_data']

st.set_page_config(page_title="🌊 Kocean 수질 분석 시스템", layout="wide", initial_sidebar_state="expanded")

st.markdown("""
<style>
.main-title {   
    font-size: 2.5rem;
    font-weight: 700;
    color: #1A5F7A;
    margin-bottom: 0.5rem;
}
.sub-title {
    font-size: 1.1rem;
    color: #576574;
    margin-bottom: 2rem;
}
</style>
""", unsafe_allow_html=True)

st.markdown('<div class="main-title">Kocean 해수 수질 종합 분석 대시보드</div>', unsafe_allow_html=True)
st.markdown('<div class="sub-title">전국 해역 및 항만별 수질 지수(WQI)를 R 엔진으로 분석하고 시각화합니다.</div>', unsafe_allow_html=True)


def _build_unique_names(names: list[str]) -> list[str]:
    seen: dict[str, int] = {}
    result: list[str] = []
    for raw in names:
        name = str(raw).strip() if raw is not None else ""
        if not name or name.lower() == "nan":
            name = "col"
        if name not in seen:
            seen[name] = 0
            result.append(name)
            continue
        seen[name] += 1
        result.append(f"{name}_{seen[name]}")
    return result

def _fill_missing_regions_raw(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    for col in out.columns:
        col_str = str(col)
        if any(k in col_str for k in ["해역", "연안", "항만", "권역", "구역"]):
            out[col] = out[col].replace(["", "nan", "NaN", "None", "none", "null", "NULL", "Null", "<NA>"], pd.NA).ffill()
    return out

def read_structured_excel(uploaded_file) -> tuple[pd.DataFrame, str | None]:
    uploaded_file.seek(0)
    raw = pd.read_excel(uploaded_file, sheet_name=0, header=None)
    if raw.empty:
        return raw, None

    if len(raw) >= 3:
        h0 = raw.iloc[0]
        h1 = raw.iloc[1]
        year_hits = h0.astype(str).str.extract(r"(20\d{2})", expand=False).dropna().tolist()
        year_label = f"{year_hits[0]}년 데이터" if year_hits else None
        has_year_like = len(year_hits) >= 3
        has_text_like = h1.astype(str).str.len().fillna(0).sum() > 0
        if has_year_like and has_text_like:
            names = _build_unique_names(h1.fillna(h0).astype(str).tolist())
            body = raw.iloc[2:].copy()
            body.columns = names
            return _fill_missing_regions_raw(body.reset_index(drop=True)), year_label

    uploaded_file.seek(0)
    normal = pd.read_excel(uploaded_file)
    return _fill_missing_regions_raw(normal), None

def hide_repeat_cells(df: pd.DataFrame, group_cols: list[str]) -> pd.DataFrame:
    out = df.copy()
    for col in group_cols:
        if col in out.columns:
            out[col] = out[col].where(out[col].ne(out[col].shift()), "")
    return out

def render_one_file(uploaded_file, idx: int) -> None:
    raw_df, year_label = read_structured_excel(uploaded_file)
    
    try:
        # R 함수를 호출하여 데이터 처리 수행 (rpy2)
        with localconverter(ro.default_converter + pandas2ri.converter):
            analyzed_df = r_process_data(raw_df)
            
        status_msg = "R 엔진(rpy2) 데이터 분석 완료"
    except Exception as e:
        st.error(f"R 데이터 처리 중 오류 발생: {e}")
        return

    # 필수 컬럼 검사
    missing = [c for c in ["temperature", "salinity", "do", "chla", "din", "dip"] if c not in analyzed_df.columns]
    if missing:
        st.error(f"데이터셋에 필수 수질 데이터가 누락되어 있습니다 (R 분석 실패).")
        return

    # 웹 네이티브 차트 및 R 통계 시각화 탭 분할
    tab_preview, tab_table, tab_chart, tab_r_chart = st.tabs(
        ["원본데이터 미리보기", "분석결과표", "항만별 WQI 차트", "R 통계 시각화 (Boxplot/DO)"]
    )

    with tab_preview:
        if year_label:
            st.info(year_label)
        st.dataframe(raw_df.head(20), use_container_width=True, hide_index=True, height=600)

    with tab_table:
        st.success(status_msg)
        s1, s2, s3 = st.columns(3)
        with s1:
            region_options = sorted([x for x in analyzed_df["region"].dropna().astype(str).unique() if str(x) != "nan"])
            region = st.selectbox("해역 선택", options=["전체"] + region_options, key=f"region_{idx}")
        with s2:
            port_options = sorted([x for x in analyzed_df["port"].dropna().astype(str).unique() if str(x) != "nan"])
            port = st.selectbox("항만/연안 선택", options=["전체"] + port_options, key=f"port_{idx}")
        with s3:
            grade_opts = [str(x) for x in analyzed_df["WQI_Grade"].dropna().unique() if str(x) != "nan"]
            grade_opts = sorted(list(set(grade_opts)))
            grade = st.selectbox("WQI 등급", options=["전체"] + grade_opts, key=f"grade_{idx}")

        port_query = st.text_input("항만검색", value="", placeholder="예: 울산", key=f"port_q_{idx}")
        sort_order = st.selectbox("정렬 기준", options=["원본 순서", "WQI 높은순", "WQI 낮은순"], key=f"sort_{idx}")
        grouped_view = st.checkbox("해역/항만 묶어보기(반복값 숨김)", value=True, key=f"group_{idx}")

        view_df = analyzed_df.copy()
        if region != "전체":
            view_df = view_df[view_df["region"].astype(str) == region]
        if port != "전체":
            view_df = view_df[view_df["port"].astype(str) == port]
        if grade != "전체":
            view_df = view_df[view_df["WQI_Grade"].astype(str) == grade]
        if port_query.strip():
            view_df = view_df[view_df["port"].astype(str).str.contains(port_query.strip(), case=False, na=False)]

        if sort_order == "원본 순서":
            view_df = view_df.sort_values("row_id")
        elif sort_order == "WQI 높은순":
            view_df = view_df.sort_values("WQI", ascending=False, na_position="last")
        else:
            view_df = view_df.sort_values("WQI", ascending=True, na_position="last")
        view_df = view_df.reset_index(drop=True)

        display_df = view_df.copy()
        display_df.insert(0, "표시순번", range(1, len(display_df) + 1))
        
        if 'date' in display_df.columns:
            display_df['date'] = pd.to_datetime(display_df['date'], errors="coerce")
            
        cols = ["표시순번", "region", "port", "date", "temperature", "salinity", "do", "chla", "din", "dip", "do_sat", "score_do", "score_chla", "score_din", "score_dip", "WQI", "WQI_Grade"]
        display_df = display_df[[c for c in cols if c in display_df.columns]].rename(
            columns={            
                "region": "해역(생태구역)",
                "port": "항만/연안",
                "date": "측정일시",
                "temperature": "수온",
                "salinity": "염분",
                "do": "DO(mg/L)",
                "chla": "Chl-a(µg/L)",
                "din": "DIN(µg/L)",
                "dip": "DIP(µg/L)",
                "do_sat": "산소포화도(%)",
                "score_do": "DO점수",
                "score_chla": "Chl-a점수",
                "score_din": "DIN점수",
                "score_dip": "DIP점수",
                "WQI": "WQI",
                "WQI_Grade": "등급",
            }
        )
        if grouped_view:
            display_df = hide_repeat_cells(display_df, ["해역(생태구역)", "항만/연안"])

        st.dataframe(
            display_df,
            use_container_width=True,
            hide_index=True,
            column_config={
                "수온": st.column_config.NumberColumn("수온", format="%.2f"),
                "염분": st.column_config.NumberColumn("염분", format="%.2f"),
                "DO(mg/L)": st.column_config.NumberColumn("DO", format="%.2f"),
                "Chl-a(µg/L)": st.column_config.NumberColumn("Chl-a", format="%.2f"),
                "DIN(µg/L)": st.column_config.NumberColumn("DIN", format="%.2f"),
                "DIP(µg/L)": st.column_config.NumberColumn("DIP", format="%.2f"),
                "산소포화도(%)": st.column_config.NumberColumn("산소포화도(%)", format="%.2f"),
                "WQI": st.column_config.NumberColumn("WQI", format="%.2f"),
                "측정일시": st.column_config.DatetimeColumn("측정일시", format="YYYY-MM-DD HH:mm"),
            },
        )

        st.download_button(
            label="분석 결과 CSV 다운로드",
            data=view_df.to_csv(index=False).encode("utf-8-sig"),
            file_name=f"{Path(uploaded_file.name).stem}_wqi_result.csv",
            mime="text/csv",
            key=f"download_{idx}",
        )

    with tab_chart:
        if analyzed_df.empty or analyzed_df["WQI"].isna().all():
            st.info("표시할 수치 데이터가 없습니다.")
            return
        
        c1, c2 = st.columns(2)
        with c1:
            st.metric("평균 WQI", f"{analyzed_df['WQI'].mean():.2f}")
        with c2:
            st.metric("데이터 건수", int(len(analyzed_df)))
            
        plot_df = analyzed_df[["port", "region", "WQI"]].dropna()
        if not plot_df.empty:
            agg_df = plot_df.groupby(["port", "region"], as_index=False)["WQI"].mean()
            agg_df = agg_df.sort_values("WQI", ascending=False)
            
            fig = px.bar(
                agg_df, 
                x="port", 
                y="WQI", 
                color="region", 
                title="항만별 WQI 차트 (R 데이터 처리 기반)",
                labels={"port": "항만/연안", "WQI": "WQI 지수", "region": "해역"}
            )
            fig.update_layout(xaxis_tickangle=-45)
            st.plotly_chart(fig, use_container_width=True, key=f"plotly_chart_{idx}")

    with tab_r_chart:
        if analyzed_df.empty:
            st.info("시각화할 데이터가 없습니다.")
        else:
            st.subheader("R 엔진 기반 정밀 통계 시각화")
            
            with tempfile.TemporaryDirectory() as tmpdir:
                box_path = Path(tmpdir) / f"boxplot_{idx}.png"
                scatter_path = Path(tmpdir) / f"scatter_{idx}.png"
                box_str = str(box_path).replace("\\", "/")
                scatter_str = str(scatter_path).replace("\\", "/")
                
                try:
                    with localconverter(ro.default_converter + pandas2ri.converter):
                        r_generate_boxplot = ro.globalenv['generate_wqi_boxplot']
                        r_generate_scatter = ro.globalenv['generate_do_temp_scatter']
                        
                        r_generate_boxplot(analyzed_df, box_str)
                        r_generate_scatter(analyzed_df, scatter_str)
                    
                    c1, c2 = st.columns(2)
                    with c1:
                        st.image(box_path.read_bytes(), caption="해역별 WQI 분포 (R Boxplot)", use_container_width=True)
                    with c2:
                        st.image(scatter_path.read_bytes(), caption="수온-용존산소(DO) 상관성 분석 (R Scatter)", use_container_width=True)
                except Exception as e:
                    st.error(f"R 차트 생성 중 오류가 발생했습니다: {e}")

uploaded_files = st.file_uploader("엑셀 파일(.xlsx) 업로드", type=["xlsx"], accept_multiple_files=True)
if not uploaded_files:
    st.info("분석을 시작하려면 하나 이상의 파일을 업로드하세요.")
    st.stop()

tabs = st.tabs([f"{i + 1}. {f.name}" for i, f in enumerate(uploaded_files)])
for i, tab in enumerate(tabs):
    with tab:
        render_one_file(uploaded_files[i], i)
