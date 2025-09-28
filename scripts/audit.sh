#!/usr/bin/env bash
# scripts/audit.sh — Refactor status audit (non-test)
# Prints a concise overview of repository state relevant to the 1.3.x refactor.
# Focus: modules, wrappers/aliases, i18n keys coverage, defun duplicates, require/provide map, LOC.

set -euo pipefail
export LC_ALL=C

# Resolve project root (prefer git, fallback to script dir/..)
ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [[ -z "${ROOT}" ]]; then
  # script dir may be scripts/, go one up
  sd="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
  ROOT="$(cd "${sd}/.." && pwd -P)"
fi
cd "${ROOT}"

LISP_DIR="lisp"

# Tools detection
have() { command -v "$1" >/dev/null 2>&1; }
RG=""
if have rg; then RG="rg"; fi

# Pretty print helpers
rule() { printf '%s\n' "----------------------------------------------------------------"; }
hdr()  { rule; printf '%s\n' "$1"; rule; }
sec()  { printf '\n'; hdr "$1"; }
kv()   { printf '  %-24s %s\n' "$1" "$2"; }

# Header
hdr "Context Navigator — Refactor Audit (non-test)"

# Repo meta
sec "Repository"
BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo '-')" || true
COMMIT="$(git rev-parse --short=12 HEAD 2>/dev/null || echo '-')" || true
kv "root"    "${ROOT}"
kv "branch"  "${BRANCH}"
kv "commit"  "${COMMIT}"

# LOC (Elisp)
sec "Lines of Code (*.el)"
if have xargs; then
  # Robust zero-delimited to handle spaces
  find "${LISP_DIR}" -type f -name '*.el' -print0 \
    | xargs -0 wc -l \
    | sed 's#\./##' \
    | sort -n
else
  # Fallback (may break on spaces)
  wc -l $(find "${LISP_DIR}" -type f -name '*.el') | sort -n
fi

# Per-file defun counts
sec "Per-file defun counts"
if [[ -n "${RG}" ]]; then
  # Count defun / cl-defun / defmacro per file
  while IFS= read -r -d '' f; do
    c="$(${RG} -n --pcre2 '^\s*\((?:cl-)?defun\b|^\s*\(defmacro\b' "${f}" | wc -l | awk '{print $1}')"
    printf '%6d  %s\n' "${c}" "${f#./}"
  done < <(find "${LISP_DIR}" -type f -name '*.el' -print0) | sort -n
else
  while IFS= read -r -d '' f; do
    c="$(egrep -n '^\s*\((cl-)?defun\b|^\s*\(defmacro\b' "${f}" | wc -l | awk '{print $1}')"
    printf '%6d  %s\n' "${c}" "${f#./}"
  done < <(find "${LISP_DIR}" -type f -name '*.el' -print0) | sort -n
fi

# Duplicate defun names (across repo)
sec "Duplicate function definitions (same name, multiple definitions)"
TMPD="$(mktemp -d)"
trap 'rm -rf "${TMPD}"' EXIT

NAMES_FILE="${TMPD}/defuns.txt"
# Robust: always use awk to avoid shell parsing issues with rg --replace
: > "${NAMES_FILE}"
while IFS= read -r -d '' f; do
  awk -v F="${f}" '
    /^[[:space:]]*\((cl-)?defun[[:space:]]+[A-Za-z0-9_-]+/ {
      match($0, /^[[:space:]]*\((cl-)?defun[[:space:]]+([A-Za-z0-9_-]+)/, m);
      if (m[2] != "") print m[2] "|" $0 "|" F;
    }' "${f}" >> "${NAMES_FILE}"
done < <(find "${LISP_DIR}" -type f -name '*.el' -print0)

# Summarize duplicates
cut -d'|' -f1 "${NAMES_FILE}" \
  | sort | uniq -c | sort -nr \
  | awk '$1>1{printf("%4d  %s\n",$1,$2)}'

# List locations for the top few duplicates
printf '\nTop duplicate definitions (locations):\n'
cut -d'|' -f1 "${NAMES_FILE}" \
  | sort | uniq -c | sort -nr | awk '$1>1{print $2}' | head -n 12 \
  | while read -r name; do
      printf "\n%s\n" "  - ${name}"
      grep -F "^${name}|" "${NAMES_FILE}" \
        | sed 's#^\([^|]*\)|\([^|]*\)|\(.*\)$#      \3 : \2#' \
        | sed 's#^\./##'
    done

# Provide/Require map
sec "Provide/Require map"
PROVIDES="${TMPD}/provides.txt"
REQUIRES="${TMPD}/requires.txt"
: > "${PROVIDES}"; : > "${REQUIRES}"

while IFS= read -r -d '' f; do
  # provides
  awk -v F="${f#./}" '
    /\(provide[[:space:]]+\047/ {
      match($0, /\(provide[[:space:]]+\047([^)[:space:]]+)/, m);
      if (m[1]!="") printf("PROVIDE  %-40s %s\n", F, m[1]);
    }' "${f}" >> "${PROVIDES}"
  # requires
  awk -v F="${f#./}" '
    /\(require[[:space:]]+\047/ {
      match($0, /\(require[[:space:]]+\047([^)[:space:]]+)/, m);
      if (m[1]!="") printf("REQUIRE  %-40s %s\n", F, m[1]);
    }' "${f}" >> "${REQUIRES}"
done < <(find "${LISP_DIR}" -type f -name '*.el' -print0)

printf '%s\n' "Provides:"; sort "${PROVIDES}"
printf '\n%s\n' "Requires:"; sort "${REQUIRES}"

# Sanity: requires missing their provide
printf '\n%s\n' "Requires with no matching provide (symbol only):"
awk '{print $NF}' "${REQUIRES}" | LC_ALL=C sort -u > "${TMPD}/req_syms.txt"
awk '{print $NF}' "${PROVIDES}" | LC_ALL=C sort -u > "${TMPD}/prov_syms.txt"
LC_ALL=C comm -23 "${TMPD}/req_syms.txt" "${TMPD}/prov_syms.txt" > "${TMPD}/req_missing.txt" || true

# Classify missing requires into external (known libs) vs project-internal
cat > "${TMPD}/externals.txt" <<'EOF'
cl-lib
subr-x
seq
json
rx
project
dired
consult
transient
transient-posframe
gptel
EOF

echo "External (skipped):"
if [[ -s "${TMPD}/req_missing.txt" ]]; then
  grep -F -x -f "${TMPD}/externals.txt" "${TMPD}/req_missing.txt" || true
fi

echo
echo "Project-internal (need provide):"
if [[ -s "${TMPD}/req_missing.txt" ]]; then
  grep -F -x -v -f "${TMPD}/externals.txt" "${TMPD}/req_missing.txt" || true
fi

# Legacy wrappers/aliases audit
sec "Legacy wrappers / aliases (presence and definitions)"
symbols=(
  context-navigator-view--wrap-segments
  context-navigator-view--footer-control-segments
  context-navigator-view--footer-control-lines
  context-navigator-view--header-toggle-lines
  context-navigator-render-build-lines
  context-navigator-render--header-lines
)
for sym in "${symbols[@]}"; do
  if [[ -n "${RG}" ]]; then
    deflocs="$(${RG} -n --pcre2 "^\s*\((?:cl-)?defun\s+${sym}\b" "${LISP_DIR}" || true)"
    # Count real code calls only: occurrences like "( symbol ..."
    refs="$(${RG} -n --pcre2 "\\([[:space:]]*${sym}\\b" "${LISP_DIR}" || true)"
  else
    deflocs="$(egrep -n "^\s*\((cl-)?defun\s+${sym}\b" -R "${LISP_DIR}" || true)"
    # Count real code calls only
    refs="$(egrep -n "\\([[:space:]]*${sym}\\b" -R "${LISP_DIR}" || true)"
  fi
  if [[ -n "${deflocs}" ]]; then
    defcnt="$(printf '%s\n' "${deflocs}" | wc -l | awk '{print $1}')"
  else
    defcnt="0"
  fi
  printf '\n%-38s (%s def)\n' "${sym}" "${defcnt}"
  if [[ -n "${deflocs}" ]]; then echo "${deflocs}" | sed 's#^\./##'; fi
  # Show first 5 references
  if [[ -n "${refs}" ]]; then
    echo "${refs}" | sed 's#^\./##' | head -n 5 | sed 's/^/    /'
  fi
done

# i18n coverage: referenced keys vs defined keys
sec "i18n keys coverage"
I18N_FILE="${LISP_DIR}/context-navigator-i18n.el"

# Build key lists into temp files (avoid fragile shell substitutions)
DEF_KEYS_FILE="${TMPD}/i18n_def_keys.txt"
REF_KEYS_FILE="${TMPD}/i18n_ref_keys.txt"

# Defined keys in dict (robust): prefer Emacs, fallback to awk
if have emacs; then
  emacs --batch -Q -L "${LISP_DIR}" \
    --eval "(progn
              (require 'context-navigator-i18n)
              (dolist (row (bound-and-true-p context-navigator-i18n--dict))
                (let ((k (car-safe row)))
                  (when (keywordp k)
                    (princ (downcase (substring (symbol-name k) 1)))
                    (terpri)))))" \
    | awk '{gsub(/^[[:space:]]+|[[:space:]\r]+$/, "", $0); if ($0 ~ /^[A-Za-z0-9-]+$/) print tolower($0)}' \
    | LC_ALL=C sort -u > "${DEF_KEYS_FILE}" || true
else
  # Fallback (line-regex; may be less precise)
  awk '
    {
      if (match($0, /^[[:space:]]*[\047(]*:([A-Za-z0-9-]+)([[:space:]]|\)|$)/, m)) print tolower(m[1]);
    }' "${I18N_FILE}" | LC_ALL=C sort -u > "${DEF_KEYS_FILE}"
fi

# Referenced keys across code (inclusive patterns):
# Prefer robust Emacs-based scan; fallback to rg/egrep when Emacs is unavailable.
I18N_METHOD=""
if have emacs; then
  I18N_METHOD="emacs"
  # Emacs: AST-сканер — читает формы и собирает ключи из:
  # - (context-navigator-i18n :key)
  # - (tr :key), если tr лексически связан с #'context-navigator-i18n
  # - (funcall tr :key) и (funcall #'context-navigator-i18n :key)
  # - (message|format|yes-or-no-p (context-navigator-i18n|tr :key) ...)
  : > "${REF_KEYS_FILE}.raw"
  # Use a temp .el to avoid fragile --eval quoting; capture stderr separately for diagnostics
  cat > "${TMPD}/scan-i18n.el" <<'ELISP'
(require 'cl-lib)

(defvar cn--scan-file-keys nil)

(defun cn--fun-sym (x)
  (cond ((symbolp x) x)
        ((and (consp x) (eq (car x) 'function) (symbolp (cadr x))) (cadr x))
        ((and (consp x) (eq (car x) 'quote) (symbolp (cadr x))) (cadr x))
        ((and (consp x) (eq (car x) 'symbol-function) (symbolp (cadr x))) (cadr x))
        (t nil)))

(defun cn--kw->name (k)
  (when (keywordp k) (substring (symbol-name k) 1)))

(defun cn--emit (k)
  (let ((name (cn--kw->name k)))
    (when name
      (princ name) (terpri)
      (push name cn--scan-file-keys))))

(defun cn--collect (form env)
  (cond
   ;; Vectors: transient layouts keep forms in vectors
   ((vectorp form)
    (dotimes (i (length form))
      (cn--collect (aref form i) env)))
   ;; Lists/cons cells
   ((consp form)
    (let ((head (car form)) (args (cdr form)))
      ;; Recurse into head when it's a cons (data structures)
      (when (consp head) (cn--collect head env))
      ;; Handle let/let* alias binding: (let ((tr #'context-navigator-i18n)) ...)
      (if (memq head '(let let*))
          (let ((bindings (car args))
                (body (cdr args))
                (env2 env))
            (dolist (b bindings)
              (when (and (consp b)
                         (eq (car b) 'tr)
                         (let ((fs (cn--fun-sym (cadr b))))
                           (eq fs 'context-navigator-i18n)))
                (setq env2 (plist-put (copy-sequence env) :tr t))))
            (dolist (x body) (cn--collect x env2)))
        (let ((fs (cn--fun-sym head)))
          (cond
           ;; Direct call
           ((eq fs 'context-navigator-i18n)
            (cn--emit (car args)))
           ;; tr alias active
           ((and (eq fs 'tr) (plist-get env :tr))
            (cn--emit (car args)))
           ;; funcall variants
           ((eq fs 'funcall)
            (let* ((callee (car args))
                   (f2 (cn--fun-sym callee))
                   (k (cadr args)))
              (cond
               ((eq f2 'context-navigator-i18n) (cn--emit k))
               ((and (eq f2 'tr) (plist-get env :tr)) (cn--emit k))))))
          ;; Wrappers: message/format/yes-or-no-p with (i18n|tr :key)
          (when (memq fs '(message format yes-or-no-p))
            (let ((f1 (car args)))
              (when (and (consp f1)
                         (let ((fs3 (cn--fun-sym (car f1))))
                           (memq fs3 '(context-navigator-i18n tr))))
                (let ((k (cadr f1)))
                  (when (or (eq (cn--fun-sym (car f1)) 'context-navigator-i18n)
                            (plist-get env :tr))
                    (cn--emit k))))))))
      ;; Recurse over args
      (when (listp args)
        (dolist (x args) (cn--collect x env)))))
   (t nil)))

(defun cn-i18n-scan-dir (dir)
  (let ((cn-trace (getenv "CN_AUDIT_I18N_TRACE")))
    (dolist (f (directory-files-recursively dir "\\.el\\'"))
      (let ((cn--scan-file-keys nil))
        (with-temp-buffer
          (insert-file-contents f)
          (goto-char (point-min))
          (condition-case _err
              (while t
                (let ((form (read (current-buffer))))
                  (cn--collect form nil)))
            (end-of-file nil)
            (error nil)))
        (when cn-trace
          (princ (format "## TRACE i18n %s %d\n" f (length (delete-dups cn--scan-file-keys)))))))))

(let ((dir (or (getenv "CN_LISP_DIR") "lisp")))
  (cn-i18n-scan-dir dir))
ELISP
  CN_LISP_DIR="${LISP_DIR}" emacs --batch -Q -L "${LISP_DIR}" -l "${TMPD}/scan-i18n.el" \
    > "${REF_KEYS_FILE}.raw" 2> "${TMPD}/emacs_i18n_err.log" || true
  # Подстраховка rg для редких форм
  if [[ -n "${RG}" ]]; then
    ${RG} --no-filename -U --pcre2 '\(context-navigator-i18n(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
      --replace '$1' >> "${REF_KEYS_FILE}.raw" || true
    ${RG} --no-filename -U --pcre2 '\([[:space:]]*tr\b(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
      --replace '$1' >> "${REF_KEYS_FILE}.raw" || true
    ${RG} --no-filename -U --pcre2 '\(funcall[[:space:]]+tr(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
      --replace '$1' >> "${REF_KEYS_FILE}.raw" || true
    ${RG} --no-filename -U --pcre2 '\(funcall(?s:.)*?context-navigator-i18n(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
      --replace '$1' >> "${REF_KEYS_FILE}.raw" || true
    ${RG} --no-filename -U --pcre2 '\((?:format|message|yes-or-no-p)[[:space:]]+\((?:context-navigator-i18n|tr)(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
      --replace '$1' >> "${REF_KEYS_FILE}.raw" || true
  fi
elif [[ -n "${RG}" ]]; then
  I18N_METHOD="rg"
  ${RG} --no-filename -U --pcre2 '\(context-navigator-i18n(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
    --replace '$1' > "${TMPD}/i18n_refs_direct.txt" || true
  ${RG} --no-filename -U --pcre2 '\(funcall[[:space:]]+tr(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
    --replace '$1' > "${TMPD}/i18n_refs_tr.txt" || true
  ${RG} --no-filename -U --pcre2 '\(funcall(?s:.)*?context-navigator-i18n(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
    --replace '$1' > "${TMPD}/i18n_refs_funcall.txt" || true
  ${RG} --no-filename -U --pcre2 '\([[:space:]]*tr\b(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
    --replace '$1' > "${TMPD}/i18n_refs_tr_alias.txt" || true
  ${RG} --no-filename -U --pcre2 '\((?:format|message|yes-or-no-p)[[:space:]]+\((?:context-navigator-i18n|tr)(?s:.)*?:([A-Za-z0-9-]+)\b' "${LISP_DIR}" \
    --replace '$1' > "${TMPD}/i18n_refs_nested.txt" || true
  cat "${TMPD}/i18n_refs_direct.txt" "${TMPD}/i18n_refs_tr.txt" "${TMPD}/i18n_refs_funcall.txt" "${TMPD}/i18n_refs_tr_alias.txt" "${TMPD}/i18n_refs_nested.txt" 2>/dev/null \
    | sort -u > "${REF_KEYS_FILE}"
else
  I18N_METHOD="grep"
  egrep -R '\(context-navigator-i18n.*:[A-Za-z0-9-]+' "${LISP_DIR}" \
    | sed -E 's/.*:([A-Za-z0-9-]+).*/\1/' > "${TMPD}/i18n_refs_direct.txt"
  egrep -R '\(funcall[[:space:]]+tr.*:[A-Za-z0-9-]+' "${LISP_DIR}" \
    | sed -E 's/.*:([A-Za-z0-9-]+).*/\1/' > "${TMPD}/i18n_refs_tr.txt"
  egrep -R '\(funcall.*context-navigator-i18n.*:[A-Za-z0-9-]+' "${LISP_DIR}" \
    | sed -E 's/.*:([A-Za-z0-9-]+).*/\1/' > "${TMPD}/i18n_refs_funcall.txt"
  egrep -R '\([[:space:]]*tr[[:space:]]+:[A-Za-z0-9-]+' "${LISP_DIR}" \
    | sed -E 's/.*:([A-Za-z0-9-]+).*/\1/' > "${TMPD}/i18n_refs_tr_alias.txt"
  cat "${TMPD}/i18n_refs_direct.txt" "${TMPD}/i18n_refs_tr.txt" "${TMPD}/i18n_refs_funcall.txt" "${TMPD}/i18n_refs_tr_alias.txt" 2>/dev/null \
    | sort -u > "${REF_KEYS_FILE}"
fi

# Sanitize referenced keys list to contain only bare key tokens (a-z0-9-)
# Keep .raw produced by Emacs/rg scan; only mv if .raw wasn't created yet.
if [[ -f "${REF_KEYS_FILE}.raw" ]]; then
  :
elif [[ -f "${REF_KEYS_FILE}" ]]; then
  mv "${REF_KEYS_FILE}" "${REF_KEYS_FILE}.raw" 2>/dev/null || true
else
  : > "${REF_KEYS_FILE}.raw"
fi
awk '{gsub(/^[[:space:]]+|[[:space:]\r]+$/, "", $0); if ($0 ~ /^[A-Za-z0-9-]+$/) print tolower($0)}' "${REF_KEYS_FILE}.raw" | LC_ALL=C sort -u > "${REF_KEYS_FILE}" || true

DEF_COUNT="$(wc -l < "${DEF_KEYS_FILE}" | awk '{print $1}')"
REF_COUNT="$(wc -l < "${REF_KEYS_FILE}" | awk '{print $1}')"

# Scanner meta/output
if [[ -f "${REF_KEYS_FILE}.raw" ]]; then
  # Count only token-like entries in raw (avoid misleading counts if raw has noisy lines)
  RAW_REF_COUNT="$(awk '{gsub(/^[[:space:]]+|[[:space:]\r]+$/,"",$0); if ($0 ~ /^[A-Za-z0-9-]+$/) c++} END{print c+0}' "${REF_KEYS_FILE}.raw")"
else
  RAW_REF_COUNT="-"
fi
printf 'Scanner method: %s\n' "${I18N_METHOD}"
printf 'Defined keys:   %s\n' "${DEF_COUNT}"
printf 'Referenced keys: %s\n' "${REF_COUNT}"
if [[ "${RAW_REF_COUNT}" != "-" ]]; then
  printf 'Referenced (raw): %s\n' "${RAW_REF_COUNT}"
fi
# Show short Emacs stderr excerpt when present (helps diagnose scan issues)
if [[ "${I18N_METHOD}" == "emacs" && -s "${TMPD}/emacs_i18n_err.log" ]]; then
  echo "Emacs scanner stderr (first 5 lines):"
  head -n 5 "${TMPD}/emacs_i18n_err.log" | sed 's/^/  /'
fi

# Debug samples to diagnose Ref=0 cases (helps tune patterns/sanitizer)
if [[ "${REF_COUNT}" -le 8 && -f "${REF_KEYS_FILE}.raw" ]]; then
  echo "Samples (raw, first 10):"
  head -n 10 "${REF_KEYS_FILE}.raw" | sed 's/^/  /'
  # If sanitizer produced anything, show a sample too (may be empty)
  if [[ -s "${REF_KEYS_FILE}" ]]; then
    echo "Samples (sanitized, first 10):"
    head -n 10 "${REF_KEYS_FILE}" | sed 's/^/  /'
  fi
fi

printf '\nMissing keys (referenced but not defined):\n'
if [[ "${REF_COUNT}" -eq 0 ]]; then
  echo "(skip) referenced keys scan returned 0 — review audit scanner patterns"
else
  LC_ALL=C comm -23 "${REF_KEYS_FILE}" "${DEF_KEYS_FILE}" || true
fi

printf '\nUnused keys (defined but not referenced):\n'
if [[ "${REF_COUNT}" -eq 0 ]]; then
  echo "(skip) suppressing massive dump while referenced=0"
else
  LC_ALL=C comm -13 "${REF_KEYS_FILE}" "${DEF_KEYS_FILE}" || true
fi

# Heuristic: i18n misuses (bare keys in message/format/yes-or-no-p/help-echo)
sec "i18n misuses (heuristic)"
if [[ -n "${RG}" ]]; then
  echo "[message/yes-or-no-p lines without i18n]"
  ${RG} -n --pcre2 '\((message|yes-or-no-p)[^)\n]*\b[A-Za-z][A-Za-z0-9-]+' "${LISP_DIR}" \
    | grep -v 'context-navigator-i18n' \
    | grep -v '/context-navigator-log\.el:' \
    | grep -v '/context-navigator\.el:' \
    | grep -v '/context-navigator-gptel-bridge\.el:' \
    | grep -v '\[context-navigator/events\]' \
    | head -n 80 || true
  echo
  echo "[help-echo bare symbols]"
  ${RG} -n "help-echo[[:space:]]+[A-Za-z][A-Za-z0-9-]+" "${LISP_DIR}" \
    | grep -v 'context-navigator-i18n' \
    | grep -Ev 'help-echo[[:space:]]+(help-str|hint)\b' \
    | head -n 40 || true
else
  echo "[message/yes-or-no-p lines without i18n]"
  egrep -nR '\((message|yes-or-no-p)[^)]*[A-Za-z][A-Za-z0-9-]+' "${LISP_DIR}" \
    | grep -v 'context-navigator-i18n' \
    | grep -v '/context-navigator-log\.el:' \
    | grep -v '/context-navigator\.el:' \
    | grep -v '/context-navigator-gptel-bridge\.el:' \
    | grep -v '\[context-navigator/events\]' \
    | head -n 80 || true
  echo
  echo "[help-echo bare symbols]"
  egrep -nR 'help-echo[[:space:]]+[A-Za-z][A-Za-z0-9-]+' "${LISP_DIR}" \
    | grep -v 'context-navigator-i18n' \
    | egrep -v 'help-echo[[:space:]]+(help-str|hint)\b' \
    | head -n 40 || true
fi

# GPTel integration audit (centralization)
sec "GPTel integration audit (bridge-centric)"
if [[ -n "${RG}" ]]; then
  echo "[bridge on-change register/unregister]"
  ${RG} -n 'context-navigator-gptel-on-change-(register|unregister)' "${LISP_DIR}" || true
  echo
  echo "[direct gptel-context* calls outside bridge]"
  ${RG} -n --pcre2 -g '!lisp/context-navigator-gptel-bridge.el' '\([[:space:]]*gptel-context\b' "${LISP_DIR}" \
    | grep -v 'memq\|quote\|:[[:alnum:]-]*gptel' || true
else
  echo "[bridge on-change register/unregister]"
  egrep -n 'context-navigator-gptel-on-change-(register|unregister)' -R "${LISP_DIR}" || true
  echo
  echo "[direct gptel-context* calls outside bridge]"
  egrep -n '\([[:space:]]*gptel-context\b' -R "${LISP_DIR}" \
    | grep -v 'context-navigator-gptel-bridge.el' \
    | grep -v 'memq\|quote\|:gptel-change' || true
fi

# Indicators/render style quick sanity
sec "Render/Indicators quick sanity"
if [[ -n "${RG}" ]]; then
  ${RG} -n 'context-navigator-render-build-item-lines|context-navigator-indicator-string' "${LISP_DIR}" || true
else
  egrep -n 'context-navigator-render-build-item-lines|context-navigator-indicator-string' -R "${LISP_DIR}" || true
fi

# TODO / FIXME sweep
sec "TODO / FIXME sweep"
if [[ -n "${RG}" ]]; then
  ${RG} -n 'TODO|FIXME' "${LISP_DIR}" || true
else
  egrep -n 'TODO|FIXME' -R "${LISP_DIR}" || true
fi

# Controls registry/order/icons consistency
sec "Controls (order/registry/icons) consistency"
ORDER_FILE="${LISP_DIR}/context-navigator-view-controls.el"
ICONS_FILE="${LISP_DIR}/context-navigator-controls-icons.el"
TMP_ORDER="${TMPD}/controls_order.txt"
TMP_REG="${TMPD}/controls_registry.txt"
TMP_ICONS="${TMPD}/controls_icons.txt"

: > "${TMP_ORDER}"; : > "${TMP_REG}"; : > "${TMP_ICONS}"

if have emacs; then
  # Extract via Emacs (robust; avoids brittle regex parsing)
  # ORDER (drop :gap)
  emacs --batch -Q -L "${LISP_DIR}" \
    --eval "(progn
              (require 'context-navigator-view-controls)
              (dolist (x (bound-and-true-p context-navigator-headerline-controls-order))
                (when (and (symbolp x) (not (eq x :gap)))
                  (princ (symbol-name x)) (terpri))))" \
    > "${TMP_ORDER}" || true

  # REGISTRY keys
  emacs --batch -Q -L "${LISP_DIR}" \
    --eval "(progn
              (require 'context-navigator-view-controls)
              (dolist (cell (bound-and-true-p context-navigator-controls-registry))
                (let ((k (car-safe cell)))
                  (when (symbolp k) (princ (symbol-name k)) (terpri)))))" \
    | sort -u > "${TMP_REG}" || true

  # ICONS keys (top-level alist keys)
  emacs --batch -Q -L "${LISP_DIR}" \
    --eval "(progn
              (require 'context-navigator-controls-icons)
              (dolist (cell (bound-and-true-p context-navigator-controls-icon-map))
                (let ((k (car-safe cell)))
                  (when (symbolp k) (princ (symbol-name k)) (terpri)))))" \
    | sort -u > "${TMP_ICONS}" || true
else
  # Fallback (best-effort) without Emacs
  if [[ -n "${RG}" ]]; then
    ${RG} --no-filename --pcre2 "(?s)defcustom\\s+context-navigator-headerline-controls-order.*?\\n\\s*'\\(([^)]*)\\)" "${ORDER_FILE}" \
      --replace '$1' \
      | tr '\n' ' ' | sed -E 's/:[A-Za-z0-9_-]+//g' \
      | tr -s '[:space:]' '\n' \
      | sed -E 's/^[[:space:]]*//;s/[[:space:]]*$//' \
      | egrep -E '^[A-Za-z0-9_-]+$' \
      | sort -u > "${TMP_ORDER}" || :

    ${RG} --no-filename --pcre2 "(?s)defcustom\\s+context-navigator-controls-registry.*?(?:\\n\\s*\\(([A-Za-z0-9_-]+)\\b)" "${ORDER_FILE}" \
      --replace '$1' | sort -u > "${TMP_REG}" || :

    ${RG} --no-filename --pcre2 "(?s)defcustom\\s+context-navigator-controls-icon-map.*?\\n\\s*\\('?\\(([A-Za-z0-9_-]+)[[:space:]]*\\.)" "${ICONS_FILE}" \
      --replace '$1' \
      | grep -Ev '^(on|off|faicon|material|octicon)$' \
      | sort -u > "${TMP_ICONS}" || :
  else
    sed -n '/defcustom[[:space:]]\+context-navigator-headerline-controls-order/,/)/p' "${ORDER_FILE}" \
      | sed -n "s/.*'(\(.*\)).*/\1/p" \
      | tr -s '[:space:]' '\n' \
      | grep -E '^[A-Za-z0-9_-]+$' \
      | grep -v '^gap$' \
      | sort -u > "${TMP_ORDER}" || :
    sed -n '/defcustom[[:space:]]\+context-navigator-controls-registry/,/)/p' "${ORDER_FILE}" \
      | sed -n 's/^[[:space:]]*(\([A-Za-z0-9_-]\+\)).*/\1/p' \
      | sort -u > "${TMP_REG}" || :
    sed -n '/defcustom[[:space:]]\+context-navigator-controls-icon-map/,/)/p' "${ICONS_FILE}" \
      | sed "s/'//g" \
      | sed -n 's/^[[:space:]]*(\([A-Za-z0-9_-]\+\)[[:space:]]*\..*/\1/p' \
      | grep -Ev '^(on|off|faicon|material|octicon)$' \
      | sort -u > "${TMP_ICONS}" || :
  fi
fi

# Normalize and sort inputs for comm (required)
LC_ALL=C sort -u -o "${TMP_ORDER}" "${TMP_ORDER}"
LC_ALL=C sort -u -o "${TMP_REG}"   "${TMP_REG}"
LC_ALL=C sort -u -o "${TMP_ICONS}" "${TMP_ICONS}"

ORDER_N=$(wc -l < "${TMP_ORDER}" | awk '{print $1}')
REG_N=$(wc -l < "${TMP_REG}" | awk '{print $1}')
ICON_N=$(wc -l < "${TMP_ICONS}" | awk '{print $1}')
kv "order keys"    "${ORDER_N}"
kv "registry keys" "${REG_N}"
kv "icon keys"     "${ICON_N}"

echo
echo "In order but missing in registry:"
LC_ALL=C comm -23 "${TMP_ORDER}" "${TMP_REG}" || true
echo
echo "In registry but not listed in order (won't render):"
LC_ALL=C comm -23 "${TMP_REG}" "${TMP_ORDER}" || true
echo
echo "Registry keys missing icon definitions:"
LC_ALL=C comm -23 "${TMP_REG}" "${TMP_ICONS}" || true
echo
echo "Icon keys unused by registry:"
# Ignore local-only icons by prefixes from Emacs when available; fallback to mf-
TMP_LOCAL="${TMPD}/icon_local_prefixes.txt"
: > "${TMP_LOCAL}"
if have emacs; then
  emacs --batch -Q -L "${LISP_DIR}" \
    --eval "(progn (require 'context-navigator-controls-icons) (dolist (p (bound-and-true-p context-navigator-controls-icon-local-prefixes)) (princ p) (terpri)))" \
    > "${TMP_LOCAL}" || true
fi
if [[ -s "${TMP_LOCAL}" ]]; then
  pat="^($(paste -sd'|' "${TMP_LOCAL}"))"
  LC_ALL=C comm -23 "${TMP_ICONS}" "${TMP_REG}" | grep -Ev "${pat}" || true
else
  LC_ALL=C comm -23 "${TMP_ICONS}" "${TMP_REG}" | grep -v '^mf-' || true
fi

# Legacy wrappers summary (orphans / dead)
sec "Legacy wrappers summary (orphans/dead)"
symbols=(
  context-navigator-view--wrap-segments
  context-navigator-view--footer-control-segments
  context-navigator-view--footer-control-lines
  context-navigator-view--header-toggle-lines
  context-navigator-render--header-lines
)

ORPHANS=""
DEAD=""

for sym in "${symbols[@]}"; do
  if [[ -n "${RG}" ]]; then
    deflocs="$(${RG} -n --pcre2 "^\s*\((?:cl-)?defun\s+${sym}\b" "${LISP_DIR}" || true)"
    # Count only code-form references: occurrences inside an S-expression "( symbol ..."
    refs="$(${RG} -n --pcre2 "\\([[:space:]]*${sym}\\b" "${LISP_DIR}" || true)"
  else
    deflocs="$(egrep -n "^\s*\((cl-)?defun\s+${sym}\b" -R "${LISP_DIR}" || true)"
    # Count only code-form references
    refs="$(egrep -n "\\([[:space:]]*${sym}\\b" -R "${LISP_DIR}" || true)"
  fi
  # Filter out likely false-positives from elisp docstrings/comments:
  # - lines that mention the symbol inside backticks =
  # - empty lines
  # This is a heuristic to ignore mentions like =symbol' in docstrings/comments.
  if [[ -n "${refs}" ]]; then
    refs="$(printf '%s\n' "${refs}" | grep -v '^[[:space:]]*;' | grep -v '\`' | grep -v '^[[:space:]]*$' || true)"
  fi
  defcnt=0; refcnt=0
  [[ -n "${deflocs}" ]] && defcnt="$(printf '%s\n' "${deflocs}" | wc -l | awk '{print $1}')" || defcnt="0"
  [[ -n "${refs}" ]]    && refcnt="$(printf '%s\n' "${refs}" | wc -l | awk '{print $1}')"    || refcnt="0"
  if [[ "${defcnt}" -eq 0 && "${refcnt}" -gt 0 ]]; then
    ORPHANS="${ORPHANS}\n  - ${sym} (${refcnt} refs)"
  fi
  if [[ "${defcnt}" -gt 0 && "${refcnt}" -eq 0 ]]; then
    DEAD="${DEAD}\n  - ${sym} (${defcnt} defs)"
  fi
done

echo "Orphan references (referenced, not defined):"
if [[ -n "${ORPHANS}" ]]; then
  printf "%b\n" "${ORPHANS}"
else
  echo "  none"
fi
echo
echo "Dead definitions (defined, not referenced):"
if [[ -n "${DEAD}" ]]; then
  printf "%b\n" "${DEAD}"
else
  echo "  none"
fi

# Refactor status summary (actionable overview)
sec "Refactor status summary"

# Duplicates (defuns)
DUP_TOTAL="$(cut -d'|' -f1 "${NAMES_FILE}" | sort | uniq -c | awk '$1>1' | wc -l | awk '{print $1}')"

# Controls diffs/counts (guard against comm errors)
CTRL_ORDER_MINUS_REG="$({ LC_ALL=C comm -23 "${TMP_ORDER}" "${TMP_REG}"   || true; } | wc -l | awk '{print $1}')"
CTRL_REG_MINUS_ORDER="$({ LC_ALL=C comm -23 "${TMP_REG}"  "${TMP_ORDER}"  || true; } | wc -l | awk '{print $1}')"
CTRL_REG_MISSING_ICONS="$({ LC_ALL=C comm -23 "${TMP_REG}"  "${TMP_ICONS}" || true; } | wc -l | awk '{print $1}')"
# Safe count without grep (pipefail-safe), ignore local mf-* icons
CTRL_ICONS_UNUSED="$({ LC_ALL=C comm -23 "${TMP_ICONS}" "${TMP_REG}" || true; } | awk 'BEGIN{c=0} !/^mf-/{c++} END{print c}')"

# i18n diffs/counts
if [[ "${REF_COUNT}" -eq 0 ]]; then
  # When referenced list is empty, avoid misleading massive "unused" number in summary.
  I18N_MISSING="0"
  I18N_UNUSED="0"
else
  I18N_MISSING="$({ LC_ALL=C comm -23 "${REF_KEYS_FILE}" "${DEF_KEYS_FILE}" || true; } | wc -l | awk '{print $1}')"
  I18N_UNUSED="$({ LC_ALL=C comm -13 "${REF_KEYS_FILE}" "${DEF_KEYS_FILE}" || true; } | wc -l | awk '{print $1}')"
fi

# requires without matching provide
REQMISS_COUNT="$(
  if [[ -s "${TMPD}/req_missing.txt" ]]; then
    grep -F -x -v -f "${TMPD}/externals.txt" "${TMPD}/req_missing.txt" | wc -l | awk '{print $1}'
  else
    echo 0
  fi
)"

# Orphans/Dead counts (from strings above)
ORPHANS_COUNT="0"
DEAD_COUNT="0"
if [[ -n "${ORPHANS}" ]]; then
  ORPHANS_COUNT="$(printf '%b\n' "${ORPHANS}" | sed -n 's/^[[:space:]]*-[[:space:]].*/x/p' | wc -l | awk '{print $1}')"
fi
if [[ -n "${DEAD}" ]]; then
  DEAD_COUNT="$(printf '%b\n' "${DEAD}" | sed -n 's/^[[:space:]]*-[[:space:]].*/x/p' | wc -l | awk '{print $1}')"
fi

# TODO/FIXME and GPTel direct calls (outside bridge)
if [[ -n "${RG}" ]]; then
  TODO_COUNT="$({ ${RG} -n 'TODO|FIXME' "${LISP_DIR}" || true; } | wc -l | awk '{print $1}')"
  GP_DIRECT_COUNT="$({ ${RG} -n --pcre2 -g '!lisp/context-navigator-gptel-bridge.el' '\([[:space:]]*gptel-context\b' "${LISP_DIR}" | grep -v 'memq\|quote\|:[[:alnum:]-]*gptel' || true; } | wc -l | awk '{print $1}')"
else
  TODO_COUNT="$({ egrep -n 'TODO|FIXME' -R "${LISP_DIR}" || true; } | wc -l | awk '{print $1}')"
  GP_DIRECT_COUNT="$({ egrep -n '\([[:space:]]*gptel-context\b' -R "${LISP_DIR}" | grep -v 'context-navigator-gptel-bridge.el' | grep -v 'memq\|quote\|:gptel-change' || true; } | wc -l | awk '{print $1}')"
fi

kv "duplicates (defuns)"                          "${DUP_TOTAL}"
kv "controls: order∖registry"                     "${CTRL_ORDER_MINUS_REG}"
kv "controls: registry∖order"                     "${CTRL_REG_MINUS_ORDER}"
kv "controls: registry missing icons"             "${CTRL_REG_MISSING_ICONS}"
kv "controls: icons unused (excl. mf-*)"          "${CTRL_ICONS_UNUSED}"
kv "i18n: missing keys"                           "${I18N_MISSING}"
kv "i18n: unused keys"                            "${I18N_UNUSED}"
kv "requires w/o provide (symbol only)"           "${REQMISS_COUNT}"
kv "legacy orphans"                               "${ORPHANS_COUNT}"
kv "legacy dead defs"                             "${DEAD_COUNT}"
kv "TODO/FIXME markers"                           "${TODO_COUNT}"
kv "gptel direct calls outside bridge"            "${GP_DIRECT_COUNT}"

# Suggestions (print only when non-zero)
sec "Suggested actions"
suggested=0
if [[ "${DUP_TOTAL}" -gt 0 ]]; then
  echo "- Remove/merge duplicate defuns (see 'Duplicate function definitions' above)"; suggested=1; fi
if [[ "${CTRL_ORDER_MINUS_REG}" -gt 0 || "${CTRL_REG_MINUS_ORDER}" -gt 0 || "${CTRL_REG_MISSING_ICONS}" -gt 0 ]]; then
  echo "- Controls: sync order/registry/icon-map (see 'Controls (order/registry/icons) consistency')"; suggested=1; fi
if [[ "${I18N_MISSING}" -gt 0 ]]; then
  echo "- i18n: add missing keys or correct call sites"; suggested=1; fi
if [[ "${I18N_UNUSED}" -gt 0 ]]; then
  echo "- i18n: consider pruning unused keys (verify first)"; suggested=1; fi
if [[ "${REQMISS_COUNT}" -gt 0 ]]; then
  echo "- Provide/Require: add missing provide or mark as external dependency"; suggested=1; fi
if [[ "${ORPHANS_COUNT}" -gt 0 || "${DEAD_COUNT}" -gt 0 ]]; then
  echo "- Legacy wrappers: remove dead defs, or update references"; suggested=1; fi
if [[ "${TODO_COUNT}" -gt 0 ]]; then
  echo "- Review TODO/FIXME list"; suggested=1; fi
if [[ "${GP_DIRECT_COUNT}" -gt 0 ]]; then
  echo "- Route direct gptel-context* calls via the bridge"; suggested=1; fi
if [[ "${suggested}" -eq 0 ]]; then
  echo "- No action items. Refactor status looks clean."
fi

# Final status line and optional strict exit
FAIL_COUNT=0
(( FAIL_COUNT += DUP_TOTAL ))
(( FAIL_COUNT += CTRL_ORDER_MINUS_REG + CTRL_REG_MINUS_ORDER + CTRL_REG_MISSING_ICONS ))
(( FAIL_COUNT += I18N_MISSING + REQMISS_COUNT + ORPHANS_COUNT + DEAD_COUNT ))
# TODO/FIXME and direct gptel-calls считаем предупреждением, не фейлом

if [[ "${FAIL_COUNT}" -gt 0 ]]; then
  kv "STATUS" "ATTENTION (${FAIL_COUNT} issue(s) detected)"
else
  kv "STATUS" "OK"
fi

# If called with --strict, return non-zero on problems
if [[ "${1:-}" == "--strict" && "${FAIL_COUNT}" -gt 0 ]]; then
  printf '\nDone (strict: FAIL).\n'
  exit 1
fi

printf '\nDone.\n'
