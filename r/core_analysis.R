# core_analysis.R
# R 언어로 작성된 핵심 데이터 전처리 및 WQI 계산 엔진

infer_region <- function(text) {
  t <- as.character(text)
  if (is.na(t)) return("기타")
  
  east <- c("동해", "포항", "울산", "속초", "강릉", "삼척", "옥계", "호산", "울릉", "대진", "거진", "아야진", "주문진", "사천", "안인", "임원", "축산", "강구", "구룡포", "온산", "방어진", "양양", "고성", "경주")
  south <- c("남해", "부산", "여수", "통영", "거제", "광양", "마산", "진해", "삼천포", "장승포", "감천", "다대포", "고흥", "보성", "순천", "목포", "완도", "해남", "강진", "장흥", "진도", "신안", "제주", "서귀포", "성산포", "화흥포", "대한해협", "서남해역")
  west <- c("서해", "인천", "평택", "당진", "대산", "군산", "보령", "태안", "장항", "경인", "안흥", "비인", "김포", "강화", "시흥", "안산", "화성", "아산", "서천", "부안", "고창", "영광", "무안", "함평", "서해중부")
  
  if (any(sapply(east, function(k) grepl(k, t)))) return("동해")
  if (any(sapply(south, function(k) grepl(k, t)))) return("남해")
  if (any(sapply(west, function(k) grepl(k, t)))) return("서해")
  
  return("기타")
}

calc_do_sat <- function(temp, sal, do_mg_l) {
  if (is.na(temp) || is.na(sal) || is.na(do_mg_l)) return(NA)
  # Weiss (1970) 공식 기반 DO 포화도(%) 산출
  T_K <- temp + 273.15
  A1 <- -173.4292; A2 <- 249.6339; A3 <- 143.3483; A4 <- -21.8492
  B1 <- -0.033096; B2 <- 0.014259; B3 <- -0.001700
  ln_do_ml <- A1 + A2 * (100 / T_K) + A3 * log(T_K / 100) + A4 * (T_K / 100) + sal * (B1 + B2 * (T_K / 100) + B3 * (T_K / 100)^2)
  do_sat_mg <- exp(ln_do_ml) * 1.42903
  return( round((do_mg_l / do_sat_mg) * 100, 2) )
}

get_wqi_base <- function(region) {
  if (region == "동해") return(list(chla=2.1, do=90, din=140, dip=20, sd=8.5))
  if (region == "서해") return(list(chla=2.2, do=90, din=425, dip=30, sd=1.0))
  # 남해 및 기타
  return(list(chla=6.3, do=90, din=220, dip=35, sd=2.5))
}

get_score <- function(val, base_val, higher_is_better=FALSE) {
  if (is.na(val) || is.na(base_val)) return(NA)
  if (higher_is_better) {
    if (val >= base_val) return(1)
    else if (val > base_val - 0.10 * base_val) return(2)
    else if (val > base_val - 0.25 * base_val) return(3)
    else if (val > base_val - 0.50 * base_val) return(4)
    else return(5)
  } else {
    if (val <= base_val) return(1)
    else if (val < base_val + 0.10 * base_val) return(2)
    else if (val < base_val + 0.25 * base_val) return(3)
    else if (val < base_val + 0.50 * base_val) return(4)
    else return(5)
  }
}

process_water_quality_data <- function(df) {
  cols <- names(df)
  lower_cols <- tolower(cols)
  
  find_col <- function(exact_cands, contains_cands = NULL) {
    for (cand in exact_cands) {
      if (cand %in% cols) return(cand)
      if (tolower(cand) %in% lower_cols) return(cols[which(tolower(cand) == lower_cols)[1]])
    }
    if (!is.null(contains_cands)) {
      for (cand in contains_cands) {
        matches <- grep(tolower(cand), lower_cols, fixed = TRUE)
        if (length(matches) > 0) return(cols[matches[1]])
      }
    }
    return(NA_character_)
  }
  
  col_region <- find_col(c("region", "Region", "해역", "권역", "구역"), c("해역/연안별(1)"))
  col_port <- find_col(c("port", "Port", "항만", "항", "항구", "지점"), c("해역/연안별(2)"))
  col_date <- find_col(c("date", "Date", "일시", "측정일시", "측정일"))
  col_temp <- find_col(c("temperature", "temp", "수온", "수온(℃)", "temperature(C)"), c("temp", "수온"))
  col_sal <- find_col(c("salinity", "염분", "염분(psu)"), c("sal", "염분"))
  col_ph <- find_col(c("ph", "pH"), c("ph"))
  col_do <- find_col(c("do", "DO", "용존산소", "DO(mg/L)", "용존산소(DO)"), c("do"))
  
  col_chla <- find_col(c("chla", "chl-a", "엽록소", "클로로필", "식물플랑크톤"), c("chl", "엽록소"))
  col_din  <- find_col(c("din", "용존무기질소", "용존무기질소(DIN)"), c("din", "용존무기질소"))
  col_dip  <- find_col(c("dip", "용존무기인", "용존무기인(DIP)"), c("dip", "용존무기인"))
  col_sd   <- find_col(c("sd", "투명도", "secchi"), c("투명도", "sd"))
  
  out <- df
  if (!is.na(col_region)) out$region <- df[[col_region]]
  if (!is.na(col_port)) out$port <- df[[col_port]]
  if (!is.na(col_date)) out$date <- df[[col_date]]
  if (!is.na(col_temp)) out$temperature <- df[[col_temp]]
  if (!is.na(col_sal)) out$salinity <- df[[col_sal]]
  if (!is.na(col_do)) out$do <- df[[col_do]]
  
  if (!is.na(col_chla)) out$chla <- df[[col_chla]] else out$chla <- NA
  if (!is.na(col_din)) out$din <- df[[col_din]] else out$din <- NA
  if (!is.na(col_dip)) out$dip <- df[[col_dip]] else out$dip <- NA
  if (!is.na(col_sd)) out$sd <- df[[col_sd]] else out$sd <- NA
  
  if (!"port" %in% names(out)) out$port <- "미분류"
  
  na_strings <- c("", "nan", "NaN", "None", "none", "null", "NULL", "Null", "<NA>")
  clean_vec <- function(v) {
    v[v %in% na_strings] <- NA
    for (i in seq_along(v)) {
      if (is.na(v[i]) && i > 1) v[i] <- v[i-1]
    }
    return(v)
  }
  out$port <- clean_vec(as.character(out$port))
  
  if (!"region" %in% names(out)) {
    out$region <- sapply(out$port, infer_region)
  } else {
    out$region <- clean_vec(as.character(out$region))
    blank_idx <- is.na(out$region)
    if (any(blank_idx)) {
      out$region[blank_idx] <- sapply(out$port[blank_idx], infer_region)
    }
    # 삭제: out$region[is.na(out$region)] <- sapply(...) 중복 및 빈 리스트 반환(NULLType 에러) 방지
  }
  
  # Ensure target columns are numeric
  for (c_name in c("temperature", "salinity", "do", "chla", "din", "dip", "sd")) {
    if (c_name %in% names(out)) {
      out[[c_name]] <- suppressWarnings(as.numeric(as.character(out[[c_name]])))
    } else {
      out[[c_name]] <- NA
    }
  }
  
  # 산소포화도(%) 및 점수 계산
  out$do_sat <- NA
  out$score_do <- NA; out$score_chla <- NA
  out$score_din <- NA; out$score_dip <- NA
  out$WQI <- NA; out$WQI_Grade <- NA
  
  for (i in seq_len(nrow(out))) {
    # DO 포화도 계산
    out$do_sat[i] <- calc_do_sat(out$temperature[i], out$salinity[i], out$do[i])
    
    # 생태구역 기준값 가져오기
    base <- get_wqi_base(out$region[i])
    
    # 각 항목 점수 (1~5)
    s_do <- get_score(out$do_sat[i], base$do, higher_is_better=TRUE)
    s_chla <- get_score(out$chla[i], base$chla, higher_is_better=FALSE)
    s_din <- get_score(out$din[i], base$din, higher_is_better=FALSE)
    s_dip <- get_score(out$dip[i], base$dip, higher_is_better=FALSE)
    
    out$score_do[i] <- s_do
    out$score_chla[i] <- s_chla
    out$score_din[i] <- s_din
    out$score_dip[i] <- s_dip
    
    # WQI 산출 공식
    # WQI = 10 * DO점수 + 6 * Chla점수 + 4 * ((DIN점수 + DIP점수) / 2)
    # 투명도 점수 제외됨
    wqi_do <- if (!is.na(s_do)) 10 * s_do else NA
    
    wqi_bio <- NA
    if (!is.na(s_chla)) wqi_bio <- 6 * s_chla
    
    wqi_nut <- NA
    if (!is.na(s_din) && !is.na(s_dip)) wqi_nut <- 4 * ((s_din + s_dip) / 2)
    else if (!is.na(s_din)) wqi_nut <- 4 * s_din
    else if (!is.na(s_dip)) wqi_nut <- 4 * s_dip
    
    if (!is.na(wqi_do) && !is.na(wqi_bio) && !is.na(wqi_nut)) {
      wqi_val <- wqi_do + wqi_bio + wqi_nut
      out$WQI[i] <- round(wqi_val, 1)
      
      # 등급 판정
      if (wqi_val <= 23) grade <- "Ⅰ등급"
      else if (wqi_val <= 33) grade <- "Ⅱ등급"
      else if (wqi_val <= 46) grade <- "Ⅲ등급"
      else if (wqi_val <= 59) grade <- "Ⅳ등급"
      else grade <- "Ⅴ등급"
      out$WQI_Grade[i] <- grade
    }
  }
  
  out$row_id <- seq_len(nrow(out))
  return(out)
}

generate_wqi_boxplot <- function(df, png_path) {
  if (.Platform$OS.type == "windows") {
    png(filename=png_path, width=800, height=500, res=110)
    par(mar=c(4, 4, 3, 2), family="Malgun Gothic")
  } else {
    png(filename=png_path, width=800, height=500, res=110, type="cairo")
    par(mar=c(4, 4, 3, 2), family="NanumGothic")
  }
  
  if (nrow(df) == 0 || all(is.na(df$WQI))) {
    plot.new(); text(0.5, 0.5, "데이터가 없거나 WQI 점수가 모두 결측치입니다.", cex=1.2)
  } else {
    base_regions <- c("동해", "남해", "서해")
    extra_regions <- setdiff(unique(as.character(df$region)), c(base_regions, "기타"))
    if ("기타" %in% df$region) all_levels <- c(base_regions, "기타", extra_regions)
    else all_levels <- c(base_regions, extra_regions)
    
    df$region <- factor(df$region, levels=all_levels)
    
    max_wqi <- max(df$WQI, na.rm=TRUE)
    if (is.infinite(max_wqi) || is.na(max_wqi)) max_wqi <- 30
    y_upper <- max_wqi + 10
    y_lower <- 18
    
    my_colors <- c("#3498db", "#2ecc71", "#e67e22", "#95a5a6", rep("gray", length(extra_regions)))
    
    boxplot(WQI ~ region, data=df,
            main="해역별 WQI 분포 (낮을수록 수질 양호)",
            xlab="해역 (Region)", ylab="WQI 지수",
            col=my_colors,
            border="gray20", las=1, outline=TRUE,
            ylim=c(y_lower, y_upper))
  }
  dev.off()
  return(png_path)
}

generate_do_temp_scatter <- function(df, png_path) {
  if (.Platform$OS.type == "windows") {
    png(filename=png_path, width=800, height=500, res=110)
    par(mar=c(4, 4, 3, 2), family="Malgun Gothic")
  } else {
    png(filename=png_path, width=800, height=500, res=110, type="cairo")
    par(mar=c(4, 4, 3, 2), family="NanumGothic")
  }
  
  if (nrow(df) == 0 || all(is.na(df$temperature)) || all(is.na(df$do_sat))) {
    plot.new(); text(0.5, 0.5, "시각화에 필요한 수온/산소포화도 데이터가 부족합니다.", cex=1.2)
  } else {
    df$region <- factor(df$region, levels=intersect(c("동해", "남해", "서해", "기타"), unique(df$region)))
    cols <- c("동해"="#3498db", "남해"="#2ecc71", "서해"="#e67e22", "기타"="#95a5a6")
    point_cols <- cols[as.character(df$region)]
    point_cols[is.na(point_cols)] <- "#95a5a6"
    
    plot(df$temperature, df$do_sat,
         main="수온-산소포화도(%) 상관 관계 분석",
         xlab="수온 (Temperature, ℃)", ylab="산소포화도 (DO sat, %)",
         col=point_cols, pch=19, cex=1.2, las=1)
         
    legend("topright", legend=levels(df$region), col=cols[levels(df$region)], pch=19, bty="n")
    
    valid <- complete.cases(df[, c("temperature", "do_sat")])
    if (sum(valid) > 2) {
      fit <- lm(do_sat ~ temperature, data=df)
      abline(fit, col="gray40", lty=2, lwd=1.5)
    }
  }
  dev.off()
  return(png_path)
}

