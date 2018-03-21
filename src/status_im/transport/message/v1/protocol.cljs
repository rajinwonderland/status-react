(ns status-im.transport.message.v1.protocol
  (:require [status-im.utils.config :as config]
            [status-im.transport.message-cache :as message-cache]
            [status-im.transport.db :as transport.db]
            [status-im.transport.core :as transport]
            [status-im.transport.message.core :as message]
            [status-im.transport.utils :as transport.utils]))

(def ^:private whisper-opts
  {:ttl       10 ;; ttl of 10 sec
   :powTarget config/pow-target
   :powTime   config/pow-time})

(defn init-chat [chat-id {:keys [db] :as cofx}]
  {:db (assoc-in db [:transport/chats chat-id] (transport.db/create-chat (transport.utils/get-topic chat-id)))})

(defn is-new? [message-id]
  (when-not (message-cache/exists? message-id)
    (message-cache/add! message-id)))

(defn requires-ack [message-id chat-id {:keys [db] :as cofx}]
  {:db (update-in db [:transport/chats chat-id :pending-ack] conj message-id)})

(defn ack [message-id chat-id {:keys [db] :as cofx}]
  {:db (update-in db [:transport/chats chat-id :ack] conj message-id)})

(defn send [{:keys [payload chat-id success-event]} {:keys [db] :as cofx}]
  ;; we assume that the chat contains the contact public-key
  (let [{:keys [current-public-key web3]} db
        {:keys [sym-key-id topic]} (get-in db [:transport/chats chat-id])]
    {:shh/post {:web3 web3
                :success-event success-event
                :message (merge {:sig current-public-key
                                 :symKeyID sym-key-id
                                 :payload payload
                                 :topic topic}
                                whisper-opts)}}))

(defn send-with-pubkey [{:keys [payload chat-id success-event]} {:keys [db] :as cofx}]
  (let [{:keys [current-public-key web3]} db
        {:keys [topic]} (get-in db [:transport/chats chat-id])]
    {:shh/post {:web3 web3
                :success-event success-event
                :message (merge {:sig current-public-key
                                 :pubKey chat-id
                                 :payload  payload
                                 :topic topic}
                                whisper-opts)}}))

(defn multi-send-with-pubkey [{:keys [payload chat-id public-keys success-event]} {:keys [db] :as cofx}]
  (let [{:keys [current-public-key web3]} db
        {:keys [topic]} (get-in db [:transport/chats chat-id])]
    {:shh/multi-post {:web3    web3
                      :success-event success-event
                      :public-keys public-keys
                      :message (merge {:sig current-public-key
                                       :payload  payload
                                       :topic topic}
                                      whisper-opts)}}))

(defrecord Ack [message-ids]
  message/StatusMessage
  (send [this cofx chat-id])
  (receive [this db chat-id sig]))

(defrecord Seen [message-ids]
  message/StatusMessage
  (send [this cofx chat-id])
  (receive [this cofx chat-id sig]))

(defrecord Message [content content-type message-type to-clock-value timestamp]
  message/StatusMessage
  (send [this chat-id cofx]
    (send {:chat-id       chat-id
           :payload       this
           :success-event [:update-message-status
                           chat-id
                           (transport.utils/message-id this)
                           (get-in cofx [:db :current-public-key])
                           :sent]}
          cofx))
  (receive [this chat-id signature cofx]
    {:dispatch [:chat-received-message/add (assoc (into {} this)
                                                  :message-id (transport.utils/message-id this)
                                                  :chat-id    chat-id
                                                  :from       signature)]}))

(defrecord MessagesSeen [message-ids]
  message/StatusMessage
  (send [this chat-id cofx]
    (send {:chat-id chat-id
           :payload this}
          cofx))
  (receive [this chat-id signature cofx]
    (message/receive-seen chat-id signature (:message-ids this) cofx)))
