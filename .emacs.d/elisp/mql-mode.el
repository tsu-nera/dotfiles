;; filename   : mql-mode.el
;; created at : Fri 06 Jul 2012 10:50:46 AM CST
;; author     : Jianing Yang <jianingy.yang AT gmail DOT com>
;; patched by : Kostafey.

;; how to write emacs mode: http://www.emacswiki.org/emacs/ModeTutorial


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mq4\\'" . mql-mode))
(add-to-list 'auto-mode-alist '("\\.mq5\\'" . mql-mode))
(add-to-list 'auto-mode-alist '("\\.mql\\'" . mql-mode))
(add-to-list 'auto-mode-alist '("\\.mqh\\'" . mql-mode))
(add-to-list 'auto-mode-alist '("\\.ex4\\'" . mql-mode))
(add-to-list 'auto-mode-alist '("\\.ex5\\'" . mql-mode))

(defvar mql-compiler-default-version 4)

(defvar mql-compiler-path
  (concat
   (cond ((eq 'gnu/linux system-type) (concat
                                       "wine \"/home/" (user-login-name)
                                       "/.wine/dosdevices/c:"))
         ((eq 'windows-nt system-type) "\"C:"))
   "/Program Files (x86)/MetaTrader 4/MQL4/mql.exe\""))

(cl-defun mql-compile (version &optional check-only)
  (compile (concat
            mql-compiler-path
            (case version
              (4 " /mql4 ")
              (5 " /mql5 "))
            (if check-only " /s " "")
            " /i:\""
            (file-name-directory (buffer-file-name)) "\" "
            (file-name-nondirectory (buffer-file-name)))))

(defun mql-compile-dispatcher ()
  "Compile mql file."
  (interactive)
  (let* ((extension (file-name-extension (buffer-file-name)))
         (version (cond ((or (equal "mq4 "extension)
                             (equal "ex4 "extension)) 4)
                        ((or (equal "mq5 "extension)
                             (equal "ex5 "extension)) 5)
                        (t mql-compiler-default-version))))
    (mql-compile version
                 (if (equal "mqh" extension) :check-only))))

(defvar mql-mode-hook nil)
(defvar mql-mode-keywords
  `(("\\<\\(?:Ask\\|B\\(?:ars\\|id\\)\\|Close\\|Digits\\|High\\|Low\\|Open\\|Point\\|\\(?:Ti\\|Volu\\)me\\)\\>"
     . font-lock-builtin-face)
    (,(concat "\\<\\(?:A\\(?:ccount\\(?:Balance\\|C\\(?:ompany\\|redit\\|ur"
             "rency\\)\\|Equity\\|FreeMargin\\(?:Check\\|Mode\\)?\\|Levera"
             "ge\\|Margin\\|N\\(?:ame\\|umber\\)\\|Profit\\|S\\(?:erver\\|"
             "topout\\(?:Level\\|Mode\\)\\)\\)\\|lert\\|rray\\(?:Bsearch\\"
             "|Copy\\(?:\\(?:Rat\\|Seri\\)es\\)?\\|Dimension\\|GetAsSeries"
             "\\|I\\(?:nitialize\\|sSeries\\)\\|M\\(?:\\(?:ax\\|in\\)imum"
             "\\)\\|R\\(?:\\(?:ang\\|esiz\\)e\\)\\|S\\(?:etAsSeries\\|ize"
             "\\|ort\\)\\)\\)\\|C\\(?:harToStr\\|omment\\)\\|D\\(?:ay\\(?:"
             "DayOfWeek\\|OfYear\\)\\|oubleToStr\\)\\|File\\(?:Close\\|Del"
             "ete\\|Flush\\|Is\\(?:\\(?:Line\\)?Ending\\)\\|Open\\(?:Histo"
             "ry\\)?\\|Read\\(?:Array\\|Double\\|Integer\\|Number\\|String"
             "\\)\\|S\\(?:eek\\|ize\\)\\|Tell\\|Write\\(?:Array\\|Double\\"
             "|Integer\\|String\\)?\\)\\|G\\(?:et\\(?:LastError\\|TickCoun"
             "t\\)\\|lobalVariable\\(?:Check\\|Del\\|Get\\|Name\\|Set\\(?:"
             "OnCondition\\)?\\|s\\(?:\\(?:DeleteAl\\|Tota\\)l\\)\\)\\)\\|"
             "H\\(?:ideTestIndicators\\|our\\)\\|I\\(?:ndicator\\(?:Buffer"
             "s\\|Counted\\|Digits\\|ShortName\\)\\|s\\(?:Connected\\|D\\("
             "?:emo\\|llsAllowed\\)\\|ExpertEnabled\\|LibrariesAllowed\\|O"
             "ptimization\\|Stopped\\|T\\(?:esting\\|rade\\(?:Allowed\\|Co"
             "ntextBusy\\)\\)\\|VisualMode\\)\\)\\|M\\(?:a\\(?:rketInfo\\|"
             "th\\(?:A\\(?:bs\\|rc\\(?:cos\\|\\(?:si\\|ta\\)n\\)\\)\\|C\\("
             "?:eil\\|os\\)\\|Exp\\|Floor\\|Log\\|M\\(?:ax\\|in\\|od\\)\\|"
             "Pow\\|R\\(?:\\(?:a\\|ou\\)nd\\)\\|S\\(?:in\\|qrt\\|rand\\)\\"
             "|Tan\\)\\)\\|essageBox\\|inute\\|onth\\)\\|NormalizeDouble\\"
             "|O\\(?:bject\\(?:Create\\|De\\(?:lete\\|scription\\)\\|Find"
             "\\|Get\\(?:FiboDescription\\|ShiftByValue\\|ValueByShift\\)?"
             "\\|Move\\|Name\\|Set\\(?:FiboDescription\\|Text\\)?\\|Type\\"
             "|s\\(?:\\(?:DeleteAl\\|Tota\\)l\\)\\)\\|rder\\(?:C\\(?:lose"
             "\\(?:By\\|\\(?:Pric\\|Tim\\)e\\)?\\|omm\\(?:ent\\|ission\\)"
             "\\)\\|Delete\\|Expiration\\|Lots\\|M\\(?:agicNumber\\|odify"
             "\\)\\|Open\\(?:\\(?:Pric\\|Tim\\)e\\)\\|Pr\\(?:\\(?:in\\|ofi"
             "\\)t\\)\\|S\\(?:e\\(?:lect\\|nd\\)\\|topLoss\\|wap\\|ymbol\\"
             ")\\|T\\(?:akeProfit\\|icket\\|ype\\)\\|s\\(?:\\(?:History\\)"
             "?Total\\)\\)\\)\\|P\\(?:eriod\\|laySound\\|rint\\)\\|Refresh"
             "Rates\\|S\\(?:e\\(?:conds\\|nd\\(?:FTP\\|Mail\\)\\|t\\(?:Ind"
             "ex\\(?:Arrow\\|Buffer\\|DrawBegin\\|EmptyValue\\|Label\\|S\\"
             "(?:hift\\|tyle\\)\\)\\|Level\\(?:\\(?:Styl\\|Valu\\)e\\)\\)"
             "\\)\\|leep\\|tr\\(?:To\\(?:Double\\|Integer\\|Time\\)\\|ing"
             "\\(?:Concatenate\\|Find\\|GetChar\\|Len\\|S\\(?:\\(?:etCha\\"
             "|ubst\\)r\\)\\|Trim\\(?:\\(?:Lef\\|Righ\\)t\\)\\)\\)\\|ymbol"
             "\\)\\|T\\(?:erminal\\(?:Company\\|Name\\|Path\\)\\|ime\\(?:C"
             "urrent\\|Day\\(?:Of\\(?:Week\\|Year\\)\\)?\\|Hour\\|Local\\|"
             "M\\(?:inute\\|onth\\)\\|Seconds\\|\\(?:ToSt\\|Yea\\)r\\)\\)"
             "\\|UninitializeReason\\|Window\\(?:BarsPerChart\\|ExpertName"
             "\\|Fi\\(?:nd\\|rstVisibleBar\\)\\|Handle\\|IsVisible\\|OnDro"
             "pped\\|Price\\(?:M\\(?:ax\\|in\\)\\|OnDropped\\)\\|Redraw\\|"
             "ScreenShot\\|TimeOnDropped\\|XOnDropped\\|YOnDropped\\|sTota"
             "l\\)\\|Year\\|i\\(?:A\\(?:DX\\|TR\\|lligator\\|[CDO]\\)\\|B"
             "\\(?:WMFI\\|a\\(?:nds\\(?:OnArray\\)?\\|r\\(?:Shift\\|s\\)\\"
             ")\\|\\(?:ear\\|ull\\)sPower\\)\\|C\\(?:CI\\(?:OnArray\\)?\\|"
             "lose\\|ustom\\)\\|DeMarker\\|Envelopes\\(?:OnArray\\)?\\|F\\"
             "(?:orce\\|ractals\\)\\|Gator\\|High\\(?:est\\)?\\|Ichimoku\\"
             "|Low\\(?:est\\)?\\|M\\(?:A\\(?:CD\\|OnArray\\)?\\|FI\\|oment"
             "um\\(?:OnArray\\)?\\)\\|O\\(?:BV\\|pen\\|sMA\\)\\|R\\(?:SI\\"
             "(?:OnArray\\)?\\|VI\\)\\|S\\(?:AR\\|t\\(?:dDev\\(?:OnArray\\"
             ")?\\|ochastic\\)\\)\\|Time\\|Volume\\|WPR\\)\\)\\>")
     . font-lock-function-name-face)))

(defun mql-source-canidates ()
  '("AccountBalance" "AccountCredit"  "AccountCompany"  "AccountCurrency"
    "AccountEquity"  "AccountFreeMargin"  "AccountFreeMarginCheck"
    "AccountFreeMarginMode"  "AccountLeverage"  "AccountMargin"  "AccountName"
    "AccountNumber"  "AccountProfit"  "AccountServer"  "AccountStopoutLevel"
    "AccountStopoutMode"  "Alert"  "ArrayBsearch"  "ArrayCopy"  "ArrayCopyRates"
    "ArrayCopySeries"  "ArrayDimension"  "ArrayGetAsSeries"  "ArrayInitialize"
    "ArrayIsSeries"  "ArrayMaximum"  "ArrayMinimum"  "ArrayRange"  "ArrayResize"
    "ArraySetAsSeries"  "ArraySize"  "ArraySort"  "CharToStr"  "Comment"
    "DayDayOfWeek"  "DayOfYear"  "DoubleToStr"  "FileClose"  "FileDelete"
    "FileFlush"  "FileIsEnding"  "FileIsLineEnding"  "FileOpen"  "FileOpenHistory"
    "FileReadArray"  "FileReadDouble"  "FileReadInteger"  "FileReadNumber"
    "FileReadString"  "FileSeek"  "FileSize"  "FileTell"  "FileWrite"
    "FileWriteArray"  "FileWriteDouble"  "FileWriteInteger"  "FileWriteString"
    "GetLastError"  "GetTickCount"  "GlobalVariableCheck"  "GlobalVariableDel"
    "GlobalVariableGet"  "GlobalVariableName"  "GlobalVariableSet"
    "GlobalVariableSetOnCondition"  "GlobalVariablesDeleteAll"
    "GlobalVariablesTotal"  "HideTestIndicators"  "Hour"  "iAC"  "iAD"
    "iAlligator"  "iADX"  "iATR"  "iAO"  "iBars"  "iBarShift"  "iBearsPower"
    "iBands"  "iBandsOnArray"  "iBullsPower"  "iBWMFI"  "iCCI"  "iCCIOnArray"
    "iClose"  "iCustom"  "iDeMarker"  "iEnvelopes"  "iEnvelopesOnArray"  "iForce"
    "iFractals"  "iGator"  "iIchimoku"  "iMomentum"  "iMomentumOnArray"  "iMFI"
    "iMA"  "iMACD"  "iMAOnArray"  "iHigh"  "iHighest"  "iLow"  "iLowest"  "iOBV"
    "iOpen"  "iOsMA"  "IndicatorBuffers"  "IndicatorCounted"  "IndicatorDigits"
    "IndicatorShortName"  "iRSI"  "iRSIOnArray"  "iRVI"  "iSAR"  "IsConnected"
    "IsDemo"  "IsDllsAllowed"  "IsExpertEnabled"  "IsLibrariesAllowed"
    "IsOptimization"  "IsStopped"  "iStdDev"  "iStdDevOnArray"  "iStochastic"
    "IsTesting"  "IsTradeAllowed"  "IsTradeContextBusy"  "IsVisualMode"  "iTime"
    "iVolume"  "iWPR"  "MarketInfo"  "MathAbs"  "MathArccos"  "MathArcsin"
    "MathArctan"  "MathCeil"  "MathCos"  "MathExp"  "MathFloor"  "MathLog"
    "MathMax"  "MathMin"  "MathMod"  "MathPow"  "MathRand"  "MathRound"  "MathSin"
    "MathSqrt"  "MathSrand"  "MathTan"  "MessageBox"  "Minute"  "Month"
    "NormalizeDouble"  "ObjectCreate"  "ObjectDelete"  "ObjectDescription"
    "ObjectFind"  "ObjectGet"  "ObjectGetFiboDescription"  "ObjectGetShiftByValue"
    "ObjectGetValueByShift"  "ObjectMove"  "ObjectName"  "ObjectsDeleteAll"
    "ObjectSet"  "ObjectSetFiboDescription"  "ObjectSetText"  "ObjectsTotal"
    "ObjectType"  "OrderClose"  "OrderCloseBy"  "OrderClosePrice"  "OrderCloseTime"
    "OrderComment"  "OrderCommission"  "OrderDelete"  "OrderExpiration"
    "OrderLots"  "OrderMagicNumber"  "OrderModify"  "OrderOpenPrice"
    "OrderOpenTime"  "OrderPrint"  "OrderProfit"  "OrderSelect"  "OrderSend"
    "OrdersHistoryTotal"  "OrderStopLoss"  "OrdersTotal"  "OrderSwap"
    "OrderSymbol"  "OrderTakeProfit"  "OrderTicket"  "OrderType"  "Period"
    "PlaySound"  "Print"  "RefreshRates"  "Seconds"  "SendFTP"  "SendMail"
    "SetIndexArrow"  "SetIndexBuffer"  "SetIndexDrawBegin"  "SetIndexEmptyValue"
    "SetIndexLabel"  "SetIndexShift"  "SetIndexStyle"  "SetLevelStyle"
    "SetLevelValue"  "Sleep"  "StringConcatenate"  "StringFind"  "StringGetChar"
    "StringLen"  "StringSetChar"  "StringSubstr"  "StringTrimLeft"
    "StringTrimRight"  "StrToDouble"  "StrToInteger"  "StrToTime"  "Symbol"
    "TerminalCompany"  "TerminalName"  "TerminalPath"  "TimeCurrent"  "TimeDay"
    "TimeDayOfWeek"  "TimeDayOfYear"  "TimeHour"  "TimeLocal"  "TimeMinute"
    "TimeMonth"  "TimeSeconds"  "TimeToStr"  "TimeYear"  "UninitializeReason"
    "WindowBarsPerChart"  "WindowExpertName"  "WindowFind"  "WindowFirstVisibleBar"
    "WindowHandle"  "WindowIsVisible"  "WindowOnDropped"  "WindowPriceMax"
    "WindowPriceMin"  "WindowPriceOnDropped"  "WindowRedraw"  "WindowScreenShot"
    "WindowTimeOnDropped"  "WindowsTotal"  "WindowXOnDropped"  "WindowYOnDropped"
    "Year" "Ask" "Bars" "Bid" "Close" "Digits" "High" "Low" "Open" "Point" "Time"
    "Volume"))

(defvar ac-source-mql
  '((candidates . mql-source-canidates)))

(defun mql-mode ()
  "Major mode for editing MetaTrader4 MQL file."
  (interactive)
  (kill-all-local-variables)
  (c-mode)
  (font-lock-add-keywords 'mql-mode mql-mode-keywords)
  (setq major-mode 'mql-mode)
  (set (make-local-variable 'ac-sources) (append ac-sources '(ac-source-mql)))
  (set (make-local-variable 'compilation-scroll-output) t)
  (setq mode-name "MQL")
  (local-set-key (kbd "C-c C-c") 'mql-compile-dispatcher)
  (run-hooks 'mql-mode-hook))

(provide 'mql-mode)
