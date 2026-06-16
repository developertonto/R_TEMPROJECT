import os
import subprocess
os.environ["R_HOME"] = r"C:\Program Files\R\R-4.5.3"
os.environ["PATH"] = r"C:\Program Files\R\R-4.5.3\bin\x64;" + os.environ.get("PATH", "")
os.environ["RPY2_CFFI_MODE"] = "ABI"
os.environ["LC_ALL"] = "C"
os.environ["LANGUAGE"] = "en"
os.environ["R_ENVIRON_USER"] = ""

import pandas as pd
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter

# Test 1: Python -> R conversion with object column of Nones
try:
    df1 = pd.DataFrame({"A": [1, 2], "B": [None, None]})
    with localconverter(ro.default_converter + pandas2ri.converter):
        ro.conversion.py2rpy(df1)
    print("Test 1 passed")
except Exception as e:
    print(f"Test 1 failed: {type(e).__name__}: {e}")

# Test 2: R -> Python conversion with empty list replacement coercion
try:
    ro.r("""
    f <- function() {
        df <- data.frame(port=c("A", "B"), region=c("X", "Y"), stringsAsFactors=FALSE)
        df$region[FALSE] <- sapply(character(0), function(x) x)
        df
    }
    """)
    r_df = ro.r['f']()
    with localconverter(ro.default_converter + pandas2ri.converter):
        pdf = ro.conversion.rpy2py(r_df)
    print("Test 2 passed")
except Exception as e:
    print(f"Test 2 failed: {type(e).__name__}: {e}")

# Test 3: Python -> R with all NA string column
try:
    df3 = pd.DataFrame({"A": [1, 2], "B": pd.Series([pd.NA, pd.NA], dtype="string")})
    with localconverter(ro.default_converter + pandas2ri.converter):
        ro.conversion.py2rpy(df3)
    print("Test 3 passed")
except Exception as e:
    print(f"Test 3 failed: {type(e).__name__}: {e}")
