;;; context-navigator-i18n.el --- Minimal i18n for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Lightweight translation table and resolver.
;; Languages: EN, RU, FR, DE, ES. Default = from locale or EN.

;;; Code:

(require 'subr-x)

(defgroup context-navigator-i18n nil
  "Internationalization settings for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-language 'auto
  "Language to use for UI strings: one of (auto en ru fr de es)."
  :type '(choice (const :tag "Auto" auto)
                 (const en) (const ru) (const fr) (const de) (const es))
  :group 'context-navigator-i18n)

(defconst context-navigator-i18n--dict
  '((:push-now
     (en . "Push gptel now")
     (ru . "Отправить в gptel")
     (fr . "Pousser maintenant")
     (de . "Jetzt senden")
     (es . "Enviar ahora"))
    (:clear-gptel
     (en . "Clear gptel")
     (ru . "Очистить gptel")
     (fr . "Effacer gptel")
     (de . "gptel leeren")
     (es . "Limpiar gptel"))
    (:global-context
     (en . "Global context")
     (ru . "Глобальный контекст")
     (fr . "Contexte global")
     (de . "Globaler Kontext")
     (es . "Contexto global"))
    (:groups
     (en . "Groups")
     (ru . "Группы")
     (fr . "Groupes")
     (de . "Gruppen")
     (es . "Grupos"))
    (:group-label
     (en . "group: %s")
     (ru . "группа: %s")
     (fr . "groupe: %s")
     (de . "Gruppe: %s")
     (es . "grupo: %s"))
    (:loading
     (en . "Loading…")
     (ru . "Загрузка…")
     (fr . "Chargement…")
     (de . "Laden…")
     (es . "Cargando…"))
    (:no-groups
     (en . "No groups (press a to add)")
     (ru . "Нет групп (нажмите a для добавления)")
     (fr . "Aucun groupe (appuyez sur a pour ajouter)")
     (de . "Keine Gruppen (drücken Sie a zum Hinzufügen)")
     (es . "No hay grupos (presione a para añadir)"))
    (:mouse-open-group
     (en . "mouse-1: open group")
     (ru . "mouse-1: открыть группу")
     (fr . "souris-1: ouvrir le groupe")
     (de . "Maus-1: Gruppe öffnen")
     (es . "ratón-1: abrir grupo"))
    (:toggle-push
     (en . "Toggle push to gptel (x)")
     (ru . "Переключить отправку в gptel (x)")
     (fr . "Basculer l’envoi vers gptel (x)")
     (de . "Umschalten: Push zu gptel (x)")
     (es . "Alternar envío a gptel (x)"))
    (:toggle-auto
     (en . "Toggle auto project switch (T)")
     (ru . "Переключить авто-переключение проекта (T)")
     (fr . "Basculer le changement auto de projet (T)")
     (de . "Autom. Projektwechsel umschalten (T)")
     (es . "Alternar cambio automático de proyecto (T)"))
    (:push-tip
     (en . "Push current items to gptel now (P)")
     (ru . "Отправить элементы в gptel сейчас (P)")
     (fr . "Envoyer les éléments vers gptel maintenant (P)")
     (de . "Elemente jetzt an gptel senden (P)")
     (es . "Enviar elementos a gptel ahora (P)"))
    (:clear-tip
     (en . "Clear gptel context (C)")
     (ru . "Очистить контекст gptel (C)")
     (fr . "Effacer le contexte gptel (C)")
     (de . "gptel-Kontext leeren (C)")
     (es . "Limpiar el contexto de gptel (C)"))
    (:items-help-view-groups
     (en . " Press h to view groups,")
     (ru . " Нажмите h, чтобы увидеть группы,")
     (fr . " Appuyez sur h pour voir les groupes,")
     (de . " Drücken Sie h, um Gruppen zu sehen,")
     (es . " Presione h para ver los grupos,"))
    (:items-help-help
     (en . "  ? for help")
     (ru . "  ? для справки")
     (fr . "  ? pour l’aide")
     (de . "  ? für Hilfe")
     (es . "  ? para ayuda"))
    ;; Transient preview / messages
    (:preview-title
     (en . "Will add %d file(s), total ~%s")
     (ru . "Будет добавлено %d файл(ов), всего ~%s")
     (fr . "Ajout de %d fichier(s), total ~%s")
     (de . "Es werden %d Datei(en) hinzugefügt, gesamt ~%s")
     (es . "Se agregarán %d archivo(s), total ~%s"))
    (:preview-skipped-too-big
     (en . "Skipped (too large): %d (limit=%s)")
     (ru . "Пропущено (слишком большие): %d (лимит=%s)")
     (fr . "Ignoré (trop volumineux): %d (limite=%s)")
     (de . "Übersprungen (zu groß): %d (Limit=%s)")
     (es . "Omitidos (demasiado grandes): %d (límite=%s)"))
    (:preview-skipped-nonregular
     (en . "Skipped (non-regular): %d")
     (ru . "Пропущено (необычные файлы): %d")
     (fr . "Ignoré (non régulier): %d")
     (de . "Übersprungen (nicht regulär): %d")
     (es . "Omitidos (no regulares): %d"))
    (:preview-remote
     (en . "Remote files: %d (TRAMP)")
     (ru . "Удалённые файлы: %d (TRAMP)")
     (fr . "Fichiers distants: %d (TRAMP)")
     (de . "Remote-Dateien: %d (TRAMP)")
     (es . "Archivos remotos: %d (TRAMP)"))
    (:preview-files
     (en . "Files:")
     (ru . "Файлы:")
     (fr . "Fichiers:")
     (de . "Dateien:")
     (es . "Archivos:"))
    (:confirm-add
     (en . "Add %d file(s) to context? ")
     (ru . "Добавить %d файл(ов) в контекст? ")
     (fr . "Ajouter %d fichier(s) au contexte ? ")
     (de . "%d Datei(en) zum Kontext hinzufügen? ")
     (es . "¿Agregar %d archivo(s) al contexto? "))
    (:no-files-selected
     (en . "No files selected in Dired")
     (ru . "В Dired ничего не выбрано")
     (fr . "Aucun fichier sélectionné dans Dired")
     (de . "Keine Dateien in Dired ausgewählt")
     (es . "No hay archivos seleccionados en Dired"))
    (:aborted
     (en . "Aborted")
     (ru . "Отменено")
     (fr . "Annulé")
     (de . "Abgebrochen")
     (es . "Cancelado"))
    (:added-files
     (en . "Added %d file(s) to context")
     (ru . "Добавлено файлов в контекст: %d")
     (fr . "Ajouté %d fichier(s) au contexte")
     (de . "%d Datei(en) zum Kontext hinzugefügt")
     (es . "Se agregaron %d archivo(s) al contexto"))
    (:warn-remote-selected
     (en . "Selected set includes %d remote file(s). Proceed? ")
     (ru . "Выбран набор с удалёнными файлами: %d. Продолжить? ")
     (fr . "La sélection inclut %d fichier(s) distant(s). Continuer ? ")
     (de . "Auswahl enthält %d Remote-Datei(en). Fortfahren? ")
     (es . "La selección incluye %d archivo(s) remoto(s). ¿Continuar? "))
    (:warn-remote-current
     (en . "Current file is remote (TRAMP). Proceed? ")
     (ru . "Текущий файл — удалённый (TRAMP). Продолжить? ")
     (fr . "Le fichier actuel est distant (TRAMP). Continuer ? ")
     (de . "Aktuelle Datei ist remote (TRAMP). Fortfahren? ")
     (es . "El archivo actual es remoto (TRAMP). ¿Continuar? "))
    (:added-selection
     (en . "Added selection to context")
     (ru . "Выделение добавлено в контекст")
     (fr . "Sélection ajoutée au contexte")
     (de . "Auswahl zum Kontext hinzugefügt")
     (es . "Selección añadida al contexto"))
    (:added-buffer
     (en . "Added buffer to context")
     (ru . "Буфер добавлен в контекст")
     (fr . "Tampon ajouté au contexte")
     (de . "Puffer zum Kontext hinzugefügt")
     (es . "Búfer agregado al contexto"))
    ;; Groups footer help (keep key hints consistent across locales)
    (:groups-help-open
     (en . " RET open")
     (ru . " RET открыть")
     (fr . " RET ouvrir")
     (de . " RET öffnen")
     (es . " RET abrir"))
    (:groups-help-add
     (en . "  a add")
     (ru . "  a добавить")
     (fr . "  a ajouter")
     (de . "  a hinzufügen")
     (es . "  a añadir"))
    (:groups-help-rename
     (en . "  r rename")
     (ru . "  r переименовать")
     (fr . "  r renommer")
     (de . "  r umbenennen")
     (es . "  r renombrar"))
    (:groups-help-delete
     (en . "  d delete")
     (ru . "  d удалить")
     (fr . "  d supprimer")
     (de . "  d löschen")
     (es . "  d eliminar"))
    (:groups-help-copy
     (en . "  c copy")
     (ru . "  c копировать")
     (fr . "  c copier")
     (de . "  c kopieren")
     (es . "  c copiar"))
    (:groups-help-refresh
     (en . "  g refresh")
     (ru . "  g обновить")
     (fr . "  g rafraîchir")
     (de . "  g aktualisieren")
     (es . "  g actualizar"))
    (:groups-help-back
     (en . "  h back")
     (ru . "  h назад")
     (fr . "  h retour")
     (de . "  h zurück")
     (es . "  h atrás"))
    (:groups-help-quit
     (en . "  q quit")
     (ru . "  q выйти")
     (fr . "  q quitter")
     (de . "  q beenden")
     (es . "  q salir"))
    (:groups-help-help
     (en . "  ? help")
     (ru . "  ? помощь")
     (fr . "  ? aide")
     (de . "  ? Hilfe")
     (es . "  ? ayuda")))
  "Translation dictionary for minimal UI strings.")

(defun context-navigator-i18n--lang-from-locale ()
  "Guess language code from environment locale."
  (let* ((lang (or (getenv "LANG") (getenv "LC_ALL") (getenv "LC_MESSAGES") "")))
    (cond
     ((string-match-p "\\`ru" lang) 'ru)
     ((string-match-p "\\`fr" lang) 'fr)
     ((string-match-p "\\`de" lang) 'de)
     ((string-match-p "\\`es" lang) 'es)
     (t 'en))))

(defun context-navigator-i18n--lang ()
  "Return current language symbol."
  (if (eq context-navigator-language 'auto)
      (context-navigator-i18n--lang-from-locale)
    context-navigator-language))

(defun context-navigator-i18n (key)
  "Return localized string for KEY."
  (let* ((lang (context-navigator-i18n--lang))
         (row (assoc key context-navigator-i18n--dict))
         (val (and row (cdr (assq lang (cdr row))))))
    (or val
        (and row (cdr (assq 'en (cdr row))))
        (format "%s" key))))

(provide 'context-navigator-i18n)
;;; context-navigator-i18n.el ends here
