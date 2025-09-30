#!/usr/bin/env bash
# scripts/rename-audit.sh — Audit residuals from module/function renames
# Focus: old→new modules (features/files) and function symbols.
# Usage:
#   bash scripts/rename-audit.sh            # report only
#   bash scripts/rename-audit.sh --strict   # exit 1 if issues detected
set -euo pipefail
export LC_ALL=C

# Resolve project root (prefer git, fallback to script dir/..)
ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [[ -z "${ROOT}" ]]; then
  sd="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
  ROOT="$(cd "${sd}/.." && pwd -P)"
fi
cd "${ROOT}"

LISP_DIR="lisp"
TEST_DIR="test"

have() { command -v "$1" >/dev/null 2>&1; }

# Search helpers (prefer ripgrep)
rg_fixed() {
  if have rg; then rg -n -F "$1" "${@:2}" || true; else grep -nR -F "$1" "${@:2}" || true; fi
}
rg_count_fixed() {
  if have rg; then
    { rg -n -F "$1" "${@:2}" || true; } | wc -l | awk '{print $1}'
  else
    { grep -nR -F "$1" "${@:2}" || true; } | wc -l | awk '{print $1}'
  fi
}
rg_pcre() {
  if have rg; then rg -n --pcre2 "$1" "${@:2}" || true; else egrep -n -R "$1" "${@:2}" || true; fi
}
rg_count_pcre() {
  if have rg; then
    { rg -n --pcre2 "$1" "${@:2}" || true; } | wc -l | awk '{print $1}'
  else
    { egrep -n -R "$1" "${@:2}" || true; } | wc -l | awk '{print $1}'
  fi
}

rule() { printf '%s\n' "----------------------------------------------------------------"; }
hdr()  { rule; printf '%s\n' "$1"; rule; }
sec()  { printf '\n'; hdr "$1"; }
kv()   { printf '  %-34s %s\n' "$1" "$2"; }

hdr "Rename Audit — modules/features/functions"

# Mapping: old-file -> new-file (module-level)
# Keep in sync with actual project renames.
declare -a FILE_MAP=(
  "lisp/context-navigator-path-add.el|lisp/context-navigator-add-paths.el"
  "lisp/context-navigator-view-const.el|lisp/context-navigator-view-constants.el"
  "lisp/context-navigator-view-ui.el|lisp/context-navigator-view-segments.el"
  "lisp/context-navigator-multifile.el|lisp/context-navigator-view-multifile.el"
  "lisp/context-navigator-modeline.el|lisp/context-navigator-view-modeline.el"
)

# Derived feature map (old-feature -> new-feature) from FILE_MAP
declare -a FEATURE_MAP=()
for pair in "${FILE_MAP[@]}"; do
  old="${pair%%|*}"; new="${pair##*|}"
  oldfeat="$(basename "${old}" .el)"
  newfeat="$(basename "${new}" .el)"
  FEATURE_MAP+=("${oldfeat}|${newfeat}")
done

# Function renames (old -> new)
declare -a FN_MAP=(
  "context-navigator-open-item|context-navigator-visit-item"
  "context-navigator-project-current-root|context-navigator-project-root"
  "context-navigator-view--render-items|context-navigator-view-render-items"
  "context-navigator-view--render-groups|context-navigator-view-render-groups"
  "context-navigator-view--items-header-toggle-lines|context-navigator-view-items-header-lines"
  "context-navigator-view--items-footer-lines|context-navigator-view-items-footer-lines"
  "context-navigator-view--groups-header-lines|context-navigator-view-groups-header-lines"
  "context-navigator-view--groups-body-lines|context-navigator-view-groups-body-lines"
)

# Paths to scan (code) and (optional) docs
SCAN_CODE_PATHS=()
[[ -d "${LISP_DIR}" ]] && SCAN_CODE_PATHS+=("${LISP_DIR}")
[[ -d "${TEST_DIR}" ]] && SCAN_CODE_PATHS+=("${TEST_DIR}")

SCAN_DOC_PATHS=()
[[ -f "README.org" ]] && SCAN_DOC_PATHS+=("README.org")
[[ -d "docs" ]] && SCAN_DOC_PATHS+=("docs")

# 1) Old files still present
sec "Old files still present (should be removed after migration)"
OLD_FILE_COUNT=0
for pair in "${FILE_MAP[@]}"; do
  old="${pair%%|*}"
  if [[ -f "${old}" ]]; then
    echo "  - ${old}"
    OLD_FILE_COUNT=$((OLD_FILE_COUNT+1))
  fi
done
[[ "${OLD_FILE_COUNT}" -eq 0 ]] && echo "  none"

# 2) Provide/Require audit for features
sec "Feature provide/require audit"
OLD_REQ_TOTAL=0
OLD_PROV_TOTAL=0
MISSING_NEW_PROVIDE=0

for pair in "${FEATURE_MAP[@]}"; do
  oldf="${pair%%|*}"
  newf="${pair##*|}"

  echo
  echo "• ${oldf} -> ${newf}"

  # Old requires in code
  req_pat="(require '${oldf})"
  rc=$(rg_count_fixed "${req_pat}" "${SCAN_CODE_PATHS[@]}")
  OLD_REQ_TOTAL=$((OLD_REQ_TOTAL+rc))
  if [[ "${rc}" -gt 0 ]]; then
    echo "  Old requires (${rc}):"
    rg_fixed "${req_pat}" "${SCAN_CODE_PATHS[@]}" | sed 's/^/    /'
  else
    echo "  Old requires: none"
  fi

  # Old provides anywhere (should be gone from codebase)
  prov_pat="(provide '${oldf})"
  pc=$(rg_count_fixed "${prov_pat}" "${LISP_DIR}")
  OLD_PROV_TOTAL=$((OLD_PROV_TOTAL+pc))
  if [[ "${pc}" -gt 0 ]]; then
    echo "  Old provides (${pc}):"
    rg_fixed "${prov_pat}" "${LISP_DIR}" | sed 's/^/    /'
  else
    echo "  Old provides: none"
  fi

  # New file provide sanity
  new_file="${LISP_DIR}/${newf}.el"
  if [[ -f "${new_file}" ]]; then
    need_prov="(provide '${newf})"
    have_prov=$(rg_count_fixed "${need_prov}" "${new_file}")
    if [[ "${have_prov}" -eq 0 ]]; then
      echo "  Missing provide in ${new_file}: ${need_prov}"
      MISSING_NEW_PROVIDE=$((MISSING_NEW_PROVIDE+1))
    else
      echo "  New file provides OK: ${newf}"
    fi
  else
    echo "  New file not found: ${new_file}"
    MISSING_NEW_PROVIDE=$((MISSING_NEW_PROVIDE+1))
  fi
done

# 3) Autoloads (loaddefs) still referencing old features
sec "Autoloads: loaddefs references to old features"
AUTO_OLD_TOTAL=0
if [[ -f "${LISP_DIR}/loaddefs.el" ]]; then
  for pair in "${FEATURE_MAP[@]}"; do
    oldf="${pair%%|*}"
    c=$(rg_count_fixed "${oldf}" "${LISP_DIR}/loaddefs.el")
    if [[ "${c}" -gt 0 ]]; then
      [[ "${AUTO_OLD_TOTAL}" -eq 0 ]] && echo "File: ${LISP_DIR}/loaddefs.el"
      AUTO_OLD_TOTAL=$((AUTO_OLD_TOTAL+c))
      rg_fixed "${oldf}" "${LISP_DIR}/loaddefs.el" | sed 's/^/  /'
    fi
  done
else
  echo "  ${LISP_DIR}/loaddefs.el not found (skip)"
fi
[[ "${AUTO_OLD_TOTAL}" -eq 0 ]] && echo "  none"

# 4) Function symbols: old names still referenced in code
sec "Function symbols: old names still referenced in code"
FN_OLD_TOTAL=0
for pair in "${FN_MAP[@]}"; do
  oldfn="${pair%%|*}"
  newfn="${pair##*|}"
  # PCRE symbol-boundary-ish: not letter/digit/hyphen on both sides
  pat="(?<![A-Za-z0-9_-])${oldfn//-/\-}(?![A-Za-z0-9_-])"
  c=$(rg_count_pcre "${pat}" "${SCAN_CODE_PATHS[@]}")
  if [[ "${c}" -gt 0 ]]; then
    FN_OLD_TOTAL=$((FN_OLD_TOTAL+c))
    echo "  - ${oldfn} -> ${newfn} (refs: ${c})"
    rg_pcre "${pat}" "${SCAN_CODE_PATHS[@]}" | head -n 8 | sed 's/^/      /'
  fi
done
[[ "${FN_OLD_TOTAL}" -eq 0 ]] && echo "  none"

# 5) Docs (optional): mentions of old features/file names in README/docs
sec "Docs scan (README/docs): old feature/file mentions"
DOC_OLD_TOTAL=0
if [[ "${#SCAN_DOC_PATHS[@]}" -gt 0 ]]; then
  for pair in "${FEATURE_MAP[@]}"; do
    oldf="${pair%%|*}"
    oldfile="${oldf}.el"
    c1=$(rg_count_fixed "${oldf}" "${SCAN_DOC_PATHS[@]}")
    c2=$(rg_count_fixed "${oldfile}" "${SCAN_DOC_PATHS[@]}")
    if [[ "${c1}" -gt 0 || "${c2}" -gt 0 ]]; then
      DOC_OLD_TOTAL=$((DOC_OLD_TOTAL+c1+c2))
      echo "  - ${oldf} (${c1}) / ${oldfile} (${c2})"
      [[ "${c1}" -gt 0 ]] && rg_fixed "${oldf}" "${SCAN_DOC_PATHS[@]}" | head -n 5 | sed 's/^/      /'
      [[ "${c2}" -gt 0 ]] && rg_fixed "${oldfile}" "${SCAN_DOC_PATHS[@]}" | head -n 5 | sed 's/^/      /'
    fi
  done
else
  echo "  no docs found (skip)"
fi
[[ "${DOC_OLD_TOTAL}" -eq 0 ]] && echo "  none"

# 6) Summary
sec "Summary"
kv "old files present"                      "${OLD_FILE_COUNT}"
kv "old requires in code (total)"           "${OLD_REQ_TOTAL}"
kv "old provides in code (total)"           "${OLD_PROV_TOTAL}"
kv "new files missing provide/count"        "${MISSING_NEW_PROVIDE}"
kv "autoloads: old symbols in loaddefs"     "${AUTO_OLD_TOTAL}"
kv "old function symbols (code refs total)" "${FN_OLD_TOTAL}"
kv "docs: old mentions total"               "${DOC_OLD_TOTAL}"

# Strict mode?
ISSUES=$(( OLD_FILE_COUNT + OLD_REQ_TOTAL + OLD_PROV_TOTAL + MISSING_NEW_PROVIDE + AUTO_OLD_TOTAL + FN_OLD_TOTAL ))
if [[ "${1:-}" == "--strict" && "${ISSUES}" -gt 0 ]]; then
  kv "STATUS" "ATTENTION (${ISSUES} issue(s) detected)"
  exit 1
fi

kv "STATUS" "OK"
exit 0
