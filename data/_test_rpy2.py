import os
import subprocess
from pathlib import Path

# rpy2 임포트 전에 환경 및 subprocess 패치 적용
os.environ["R_HOME"] = r"C:\Program Files\R\R-4.5.3"
os.environ["PATH"] = r"C:\Program Files\R\R-4.5.3\bin\x64;" + os.environ["PATH"]
os.environ["RPY2_CFFI_MODE"] = "ABI"
os.environ["LC_ALL"] = "C"
os.environ["LANGUAGE"] = "en"
os.environ["R_ENVIRON_USER"] = ""

_orig = subprocess.run
def _safe_run(cmd, *args, **kwargs):  # type: ignore
    if kwargs.get("text") and "encoding" not in kwargs:
        kwargs["errors"] = kwargs.get("errors", "replace")
    return _orig(cmd, *args, **kwargs)
subprocess.run = _safe_run  # type: ignore

import rpy2.robjects as ro

ro.r("""
wqi_calc <- function(temperature, salinity, ph, do_val) {
  clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))
  s_temp <- clamp(100 - abs(temperature - 20) * 5, 0, 100)
  s_sal  <- clamp(100 - abs(salinity - 33) * 3,    0, 100)
  s_ph   <- clamp(100 - abs(ph - 8.1) * 30,        0, 100)
  s_do   <- clamp((do_val / 10) * 100,              0, 100)
  wqi    <- round(0.25*s_temp + 0.25*s_sal + 0.25*s_ph + 0.25*s_do, 2)
  grade  <- ifelse(wqi >= 90, "1", ifelse(wqi >= 75, "2", ifelse(wqi >= 60, "3", "4")))
  list(WQI=wqi, WQI_Grade=grade)
}
""")

res = ro.r["wqi_calc"](
    ro.FloatVector([16.2, 15.5]),
    ro.FloatVector([33.6, 33.0]),
    ro.FloatVector([8.1, 7.9]),
    ro.FloatVector([9.2, 8.5]),
)
print("WQI   :", list(res.rx2("WQI")))
print("Grade :", list(res.rx2("WQI_Grade")))
print("rpy2 WQI 계산 OK")

# --- 그래프 테스트 ---
import tempfile

ports  = ["울산항", "부산항", "포항항", "울산항", "부산항"]
wqis   = [92.0, 78.5, 85.0, 90.0, 80.0]

with tempfile.TemporaryDirectory() as td:
    png_path = Path(td) / "test_plot.png"
    png_str = str(png_path).replace("\\", "/")

    ro.r.assign("r_port", ro.StrVector(ports))
    ro.r.assign("r_wqi",  ro.FloatVector(wqis))
    ro.r.assign("r_png",  ro.StrVector([png_str]))

    ro.r("""
    plot_df_r <- data.frame(port=r_port, WQI=r_wqi, stringsAsFactors=FALSE)
    agg <- aggregate(WQI ~ port, data=plot_df_r, FUN=mean)
    agg <- agg[order(agg$WQI, decreasing=TRUE), ]
    png(filename=r_png[1], width=1100, height=620, res=110)
    par(mar=c(11, 5, 4, 2), family="sans")
    bp <- barplot(
      agg$WQI,
      names.arg = agg$port,
      las = 2,
      col = ifelse(agg$WQI >= 90, "#2ecc71",
             ifelse(agg$WQI >= 75, "#3498db",
               ifelse(agg$WQI >= 60, "#f39c12", "#e74c3c"))),
      ylim = c(0, 100),
      ylab = "WQI",
      main = "R 기반 항만/연안별 평균 WQI",
      border = NA
    )
    text(bp, agg$WQI + 1.5, labels=round(agg$WQI, 1), cex=0.75, col="gray20")
    dev.off()
    """)
    size = png_path.stat().st_size
    print(f"PNG 생성 완료: {size} bytes")
