
;; For historical reasons, Gnus has a single "primary" select method and multiple "secondary" select methods. The only difference is that the primary select method has a default (tries your local news server), and the groups (folders) in it are not prefixed with the server name. 
;; Because I don't like to put anyone on a piedestal. I choose to make the primary select method one that does nothing - that way all the "real" servers are equal. 
; No primary server:
(setq gnus-select-method '(nnnil ""))

; Set name and email:
(setq user-full-name "Moamen Hraden"
      user-mail-address "moamenhredeen@gmail.com")


; Get email, and store in nnml:
(setq gnus-secondary-select-methods
	  '((nntp "news.gwene.org")
		(nnimap "gmail"
				(nnimap-address "imap.gmail.com")
				(nnimap-server-port "imaps")
				(nnimap-stream ssl))
		;; (nnimap "guad"
		;; 		(nnimap-address "outlook.office365.com")
		;; 		(nnimap-server-port "993")
		;; 		(nnimap-stream ssl))
		))


(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)


; Use fancy splitting:
(setq nnmail-split-methods 'nnmail-split-fancy)


; Use topics per default:
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

; Smileys:
(setq smiley-style 'medium)

; Don't get the first article automatically:
(setq gnus-auto-select-first nil)

; Don't show that annoying arrow:
(setq gnus-summary-display-arrow nil)

; sort emails and articles by date
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)))


(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
