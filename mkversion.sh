(echo "\\newcommand{\\version}{"`(git describe --always --long --dirty 2>/dev/null || echo "unknown")`"}")  > version.tex
