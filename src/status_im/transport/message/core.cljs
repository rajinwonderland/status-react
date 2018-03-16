(ns status-im.transport.message.core
  (:require [status-im.utils.handlers :as handlers]
            [status-im.chat.models :as models.chat]))

(defprotocol StatusMessage
  "Protocol for transport layed status messages"
  (send [this chat-id cofx])
  (receive [this chat-id signature cofx]))

;; TODO (yenda) implement
;; :pending
;; :discover
;; :discoveries-request
;; :discoveries-response
;; :profile
;; :online


;;TODO (yenda) this is probably not the place to have these
(defn receive-contact-request
  [public-key
   {:keys [name profile-image address fcm-token]}
   {{:contacts/keys [contacts] :as db} :db :as cofx}]
  (when-not (get contacts public-key)
    (let [contact-props {:whisper-identity public-key
                         :public-key       public-key
                         :address          address
                         :photo-path       profile-image
                         :name             name
                         :fcm-token        fcm-token
                         :pending?         true}
          chat-props    {:name         name
                         :chat-id      public-key
                         :contact-info (prn-str contact-props)}]
      (handlers/merge-fx cofx
                         {:db           (update-in db [:contacts/contacts public-key] merge contact-props)
                          :save-contact contact-props}
                         (models.chat/add-chat public-key chat-props)))))

(defn receive-contact-request-confirmation
  [public-key {:keys [name profile-image address fcm-token]}
   {{:contacts/keys [contacts] :as db} :db :as cofx}]
  (when-let [contact (get contacts public-key)]
    (let [contact-props {:whisper-identity public-key
                         :address          address
                         :photo-path       profile-image
                         :name             name
                         :fcm-token        fcm-token}
          chat-props    {:name    name
                         :chat-id public-key}]
      (handlers/merge-fx cofx
                         {:db           (update-in db [:contacts/contacts public-key] merge contact-props)
                          :save-contact contact-props}
                         (models.chat/upsert-chat chat-props)))))

(defn receive-seen
  [chat-id sender message-ids {:keys [db]}]
  (when-let [seen-messages-ids (-> (get-in db [:chats chat-id :messages])
                                   (select-keys message-ids)
                                   keys)]
    {:db (reduce (fn [new-db message-id]
                   (assoc-in new-db [:chats chat-id :messages message-id :user-statuses sender] :seen))
                 db
                 seen-messages-ids)}))


;;TODO(yenda) this needs to be fixed, refactored from old protocol, symbols might be undeclared/missing/wrong
(defn save-system-message! [message-id chat-id timestamp content]
  {:from         "system"
   :message-id   message-id
   :chat-id      chat-id
   :timestamp    timestamp
   :content      content
   :content-type constants/text-content-type})

(re-frame/reg-fx
  :data-store.chats/add-contact
  (fn [[chat-id identity]]
    (chats/add-contacts chat-id [identity])))

(re-frame/reg-fx
  :data-store.chats/remove-contact
  (fn [[chat-id identity]]
    (chats/remove-contacts chat-id [identity])))

(re-frame/reg-fx
  ::participant-added-message
  (fn [{:keys [chat-id current-identity identity from message-id timestamp contacts]}]
    (let [inviter-name (get-in contacts [from :name])
          invitee-name (if (= identity current-identity)
                         (i18n/label :t/You)
                         (get-in contacts [identity :name]))]
      (save-system-message! message-id
                            chat-id
                            timestamp
                            (str (or inviter-name from) " " (i18n/label :t/invited) " " (or invitee-name identity))))))

(re-frame/reg-fx
  ::you-removed-message
  (fn [{:keys [from message-id timestamp chat-id contacts]}]
    (let [remover-name (get-in contacts [from :name])]
      (save-system-message! message-id
                            chat-id
                            timestamp
                            (str (or remover-name from) " " (i18n/label :t/removed-from-chat))))))

(re-frame/reg-fx
  ::participant-removed-message
  (fn [{:keys [identity from message-id timestamp chat-id contacts]}]
    (let [remover-name (get-in contacts [from :name])
          removed-name (get-in contacts [identity :name])]
      (save-system-message! message-id
                            chat-id
                            timestamp
                            (str (or remover-name from) " " (i18n/label :t/removed) " " (or removed-name identity))))))

(re-frame/reg-fx
  ::participant-left-message
  (fn [{:keys [chat-id from message-id timestamp contacts]}]
    (let [left-name (get-in contacts [from :name])
          message-text (str (or left-name from) " " (i18n/label :t/left))]
      (save-system-message! message-id groupe-id timestamp message-text))))



(defn participant-added [signature cofx]
  (let [{:keys [current-public-key chats contacts/contacts] :as db} (:db cofx)
        chat  (get-in db [:chats chat-id])
        admin (:group-admin chats)]
    (when (= from admin)
      (merge {::participant-added-message {:chat-id chat-id
                                           :current-public-key current-public-key
                                           :identity identity
                                           :from from
                                           :message-id message-id
                                           :timestamp timestamp
                                           :contacts contacts}}
             (when-not (and (= current-public-key identity)
                            (has-contact? chat identity))
               {:db (update-in db [:chats chat-id :contacts] conj {:identity identity})
                :data-store.chats/add-contact [chat-id identity]})))))

(defn participant-removed [signature cofx]
  (fn [{{:keys [current-public-key chats contacts/contacts]} :db message-by-id :message-by-id}
       [{:keys                                              [from]
         {:keys [chat-id identity message-id timestamp]}   :payload
         :as                                                message}]]
    (when-not message-by-id
      (let [admin (get-in chats [chat-id :group-admin])]
        (when (= admin from)
          (if (= current-public-key identity)
            ;; you have been removed from group
            (let [chat        (get-in db [:chats chat-id])
                  new-update? (chat/new-update? chat timestamp)]
              (when new-update?
                {::you-removed-message {:from       from
                                        :message-id message-id
                                        :timestamp  timestamp
                                        :chat-id    chat-id
                                        :contacts   contacts}
                 ;; TODO (yenda) could it be avoided to dispatch that ?
                 :dispatch             [:update-chat! {:chat-id         chat-id
                                                       :removed-from-at timestamp
                                                       :is-active       false}]}))
            ;; other participant has been removed from group
            {::participant-removed-message {:identity   identity
                                            :from       from
                                            :message-id message-id
                                            :timestamp  timestamp
                                            :chat-id    chat-id
                                            :contacts   contacts}
             :data-store.chats/remove-contact [chat-id identity]}))))))

(defn participant-left [signature cofx]
  (let [{:keys [current-public-key contacts/contacts] :as db} (:db cofx)
        chat (get-in db [:chats chat-id])
        {chat-is-active :is-active chat-timestamp :timestamp} chat]
    (when (and (not= current-public-key from)
               chat-is-active
               (> timestamp chat-timestamp))
      {::participant-left-group-message {:chat-id    chat-id
                                         :from       from
                                         :message-id message-id
                                         :timestamp  timestamp
                                         :contacts   contacts}
       :data-store.chats/remove-contact [chat-id from]
       :db (update-in db [:chats chat-id :contacts]
                      #(remove (fn [{:keys [identity]}]
                                 (= identity from)) %))})))
