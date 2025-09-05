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
  '((:add-from-text
     (en . "Add files from text")
     (ru . "Добавить файлы из текста"))
    (:add-from-minibuf
     (en . "Add files from minibuffer")
     (ru . "Добавить файлы из minibuffer"))
    (:resolve-start
     (en . "Resolving names...")
     (ru . "Идёт сопоставление..."))
    (:resolve-summary
     (en . "Resolved %d file(s); skipped: too big=%d, non-regular=%d")
     (ru . "Найдено %d файл(ов); пропущено: слишком большие=%d, нерегулярные=%d"))
    (:ambiguous-found
     (en . "Ambiguities detected:")
     (ru . "Обнаружены неоднозначности:"))
    (:unresolved-found
     (en . "Unresolved:")
     (ru . "Не удалось разрешить:"))
    (:too-many
     (en . "Too many files (%d), max %d — operation cancelled")
     (ru . "Слишком много файлов (%d), максимум %d — операция отменена"))
    (:remote-warning
     (en . "Remote file(s) will be added: %d. Proceed? ")
     (ru . "Будут добавлены удалённые файлы: %d. Продолжить? "))
    (:added-files-limited
     (en . "Added %d file(s) (limited)")
     (ru . "Добавлено %d файл(ов) (с ограничением)"))
    (:push-now
     (en . "push now")
     (ru . "в gptel")
     (fr . "pousser maintenant")
     (de . "jetzt senden")
     (es . "enviar ahora"))
    (:clear-gptel
     (en . "clear gptel")
     (ru . "очистить gptel")
     (fr . "effacer gptel")
     (de . "gptel leeren")
     (es . "limpiar gptel"))
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
    (:open-buffers
     (en . "open buffers")
     (ru . "открыть буферы")
     (fr . "ouvrir les tampons")
     (de . "puffer öffnen")
     (es . "abrir búferes"))
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
    (:auto-proj
     (en . "auto-proj")
     (ru . "авто-проект")
     (fr . "auto-proj")
     (de . "auto-proj")
     (es . "auto-proj"))
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
     (es . "  ? ayuda"))
    ;; Help (sidebar) — localized labels and section titles
    (:help-title
     (en . "Context Navigator — keys:")
     (ru . "Context Navigator — клавиши:")
     (fr . "Context Navigator — raccourcis :")
     (de . "Context Navigator — Tasten:")
     (es . "Context Navigator — teclas:"))
    (:help-global-title
     (en . "Global keys (context-navigator-mode):")
     (ru . "Глобальные клавиши (context-navigator-mode):")
     (fr . "Raccourcis globaux (context-navigator-mode) :")
     (de . "Globale Tasten (context-navigator-mode):")
     (es . "Teclas globales (context-navigator-mode):"))
    (:help-global-summary
     (en . "C-c n  transient: n panel, p project, a add, g groups, s save, l load, u unload, x push, T auto, P push-now, C clear")
     (ru . "C-c n  transient: n панель, p проект, a добавить, g группы, s сохранить, l загрузить, u выгрузить, x отправка, T авто, P отправить, C очистить")
     (fr . "C-c n  transient : n panneau, p projet, a ajouter, g groupes, s sauver, l charger, u décharger, x push, T auto, P push-maintenant, C effacer")
     (de . "C-c n  Transient: n Panel, p Projekt, a hinzufügen, g Gruppen, s speichern, l laden, u entladen, x push, T auto, P jetzt-senden, C leeren")
     (es . "C-c n  transient: n panel, p proyecto, a añadir, g grupos, s guardar, l cargar, u descargar, x push, T auto, P enviar-ahora, C limpiar"))
    (:help-groups-summary
     (en . "Groups mode keys: RET open, a add, r rename, d delete, c copy, g refresh, h back, q quit")
     (ru . "Клавиши режима групп: RET открыть, a добавить, r переименовать, d удалить, c копировать, g обновить, h назад, q выйти")
     (fr . "Touches du mode groupes : RET ouvrir, a ajouter, r renommer, d supprimer, c copier, g rafraîchir, h retour, q quitter")
     (de . "Gruppenmodus-Tasten: RET öffnen, a hinzufügen, r umbenennen, d löschen, c kopieren, g aktualisieren, h zurück, q beenden")
     (es . "Teclas del modo de grupos: RET abrir, a añadir, r renombrar, d eliminar, c copiar, g actualizar, h atrás, q salir"))
    (:help-next-item
     (en . "next item")
     (ru . "следующий элемент")
     (fr . "élément suivant")
     (de . "nächstes Element")
     (es . "siguiente elemento"))
    (:help-previous-item
     (en . "previous item")
     (ru . "предыдущий элемент")
     (fr . "élément précédent")
     (de . "voriges Element")
     (es . "elemento anterior"))
    (:help-activate
     (en . "RET: visit item / open group")
     (ru . "RET: перейти к элементу / открыть группу")
     (fr . "RET : visiter l’élément / ouvrir le groupe")
     (de . "RET: Element öffnen / Gruppe öffnen")
     (es . "RET: visitar elemento / abrir grupo"))
    (:help-preview
     (en . "preview item (other window)")
     (ru . "просмотр элемента (другое окно)")
     (fr . "prévisualiser l’élément (autre fenêtre)")
     (de . "Element in anderer Fensteransicht anzeigen")
     (es . "previsualizar elemento (otra ventana)"))
    (:help-toggle-gptel
     (en . "toggle gptel membership")
     (ru . "переключить членство в gptel")
     (fr . "basculer l’appartenance à gptel")
     (de . "gptel-Mitgliedschaft umschalten")
     (es . "alternar pertenencia a gptel"))
    (:help-delete
     (en . "delete (item or group)")
     (ru . "удалить (элемент или группу)")
     (fr . "supprimer (élément ou groupe)")
     (de . "löschen (Element oder Gruppe)")
     (es . "eliminar (elemento o grupo)"))
    (:help-refresh
     (en . "refresh (items or groups)")
     (ru . "обновить (элементы или группы)")
     (fr . "rafraîchir (éléments ou groupes)")
     (de . "aktualisieren (Elemente oder Gruppen)")
     (es . "actualizar (elementos o grupos)"))
    (:help-go-up
     (en . "show groups list")
     (ru . "показать список групп")
     (fr . "afficher la liste des groupes")
     (de . "Gruppenliste anzeigen")
     (es . "mostrar lista de grupos"))
    (:help-group-create
     (en . "add group")
     (ru . "добавить группу")
     (fr . "ajouter un groupe")
     (de . "Gruppe hinzufügen")
     (es . "añadir grupo"))
    (:help-group-rename
     (en . "rename group")
     (ru . "переименовать группу")
     (fr . "renommer le groupe")
     (de . "Gruppe umbenennen")
     (es . "renombrar grupo"))
    (:help-group-duplicate
     (en . "duplicate group")
     (ru . "дублировать группу")
     (fr . "dupliquer le groupe")
     (de . "Gruppe duplizieren")
     (es . "duplicar grupo"))
    (:help-toggle-push
     (en . "toggle push → gptel")
     (ru . "переключить отправку → gptel")
     (fr . "basculer l’envoi → gptel")
     (de . "Umschalten: Push → gptel")
     (es . "alternar envío → gptel"))
    (:help-toggle-auto
     (en . "toggle auto-project switching")
     (ru . "переключить авто-переключение проекта")
     (fr . "basculer le changement auto de projet")
     (de . "automatischen Projektwechsel umschalten")
     (es . "alternar cambio automático de proyecto"))
    (:help-open-all
     (en . "open all context buffers (background)")
     (ru . "открыть все буферы контекста (в фоне)")
     (fr . "ouvrir tous les tampons du contexte (arrière-plan)")
     (de . "alle Kontextpuffer öffnen (Hintergrund)")
     (es . "abrir todos los búferes del contexto (segundo plano)"))
    (:help-push-now
     (en . "push now to gptel (when auto-push is off)")
     (ru . "отправить сейчас в gptel (если авто-отправка выключена)")
     (fr . "envoyer maintenant vers gptel (si l’envoi auto est désactivé)")
     (de . "jetzt an gptel senden (wenn Auto-Push aus ist)")
     (es . "enviar ahora a gptel (si el envío auto está desactivado)"))
    (:help-clear-gptel
     (en . "clear gptel context")
     (ru . "очистить контекст gptel")
     (fr . "effacer le contexte gptel")
     (de . "gptel-Kontext leeren")
     (es . "limpiar el contexto de gptel"))
    (:help-quit
     (en . "quit sidebar")
     (ru . "выйти из боковой панели")
     (fr . "quitter le panneau latéral")
     (de . "Seitenleiste schließen")
     (es . "salir de la barra lateral"))
    (:help-help
     (en . "show this help")
     (ru . "показать эту справку")
     (fr . "afficher cette aide")
     (de . "diese Hilfe anzeigen")
     (es . "mostrar esta ayuda")))
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
