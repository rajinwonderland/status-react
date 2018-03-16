(ns status-im.transport.message.v1.group-chat
  (:require [re-frame.core :as re-frame]
            [status-im.utils.handlers :as handlers]
            [status-im.transport.message.core :as message]
            [status-im.transport.message.v1.protocol :as protocol]
            [status-im.transport.utils :as transport.utils]
            [status-im.data-store.chats :as chats]))


;; NOTE: We ignore the chat-id from the send and receive method. The chat-id is usually
;; deduced from the filter the message comes from but not in that case because it is sent
;; individually to each participant of the group.
;; In order to be able to determine which group the messge belongs to the chat-id is therefore
;; passed in the message itself
(defrecord NewGroupKey [chat-id sym-key message]
  message/StatusMessage
  (send [this _ cofx]
    (protocol/send-to-many-with-pubkey {:chat-id chat-id
                                        :payload this}
                                       cofx))
  (receive [this _ signature cofx]
    (handlers/merge-fx cofx
                       {:shh/add-new-sym-key {:web3       (get-in cofx [:db :web3])
                                              :sym-key    sym-key
                                              :on-success (fn [sym-key sym-key-id]
                                                            (re-frame/dispatch [::add-new-sym-key {:chat-id    chat-id
                                                                                                   :sym-key    sym-key
                                                                                                   :sym-key-id sym-key-id
                                                                                                   :message    message}]))}}
                       (protocol/init-chat chat-id))))

(defn send-new-group-key [message participants cofx]
  (when admin?
    {:shh/get-new-sym-key {:web3 (get-in cofx [:db :web3])
                           :on-success (fn [sym-key sym-key-id]
                                         (re-frame/dispatch [::send-new-sym-key {:chat-ids   participants
                                                                                 :sym-key    sym-key
                                                                                 :sym-key-id sym-key-id
                                                                                 :message    message}]))}}))

(defrecord GroupAdminUpdate [participants]
  message/StatusMessage
  (send [this chat-id cofx]
    ;;TODO send to many with NewGroupKey
    (send-new-group-key this participants))
  (receive [this chat-id signature cofx]
    (message/participant-added signature cofx)))

(defrecord GroupLeave []
  message/StatusMessage
  (send [this chat-id cofx]
    ;;TODO send to group
  (receive [this chat-id signature cofx]
    (handlers/merge-fx cofx
                       (update-group-key message participants)
                       (message/participant-left signature))))

(handlers/register-handler-fx
  ::send-new-sym-key
  ;; this is the event that is called when we want to send a message that required first
  ;; some async operations
  (fn [{:keys [db] :as cofx} [_ {:keys [chat-id message sym-key sym-key-id]}]]
    (let [{:keys [web3]} db]
      (handlers/merge-fx cofx
                         {:db (assoc-in db [:transport/chats chat-id :sym-key-id] sym-key-id)
                          :shh/add-filter {:web3 web3
                                           :sym-key-id sym-key-id
                                           :topic (transport.utils/get-topic chat-id)
                                           :chat-id chat-id}
                          :data-store.transport/save {:chat-id chat-id
                                                      :chat (-> (get-in db [:transport/chats chat-id])
                                                                (assoc :sym-key-id sym-key-id)
                                                                ;;TODO (yenda) remove once go implements persistence
                                                                (assoc :sym-key sym-key))}}
                         (message/send (NewGroupKey. chat-id sym-key message) chat-id)))))

(handlers/register-handler-fx
  ::add-new-sym-key
  (fn [{:keys [db] :as cofx} [_ {:keys [sym-key-id sym-key chat-id message]}]]
    (let [{:keys [web3 current-public-key]} db]
      (handlers/merge-fx cofx
                         {:db (assoc-in db [:transport/chats chat-id :sym-key-id] sym-key-id)
                          :shh/add-filter {:web3 web3
                                           :sym-key-id sym-key-id
                                           :topic (transport.utils/get-topic current-public-key)
                                           :chat-id chat-id}
                          :data-store.transport/save {:chat-id chat-id
                                                      :chat (-> (get-in db [:transport/chats chat-id])
                                                                (assoc :sym-key-id sym-key-id)
                                                                ;;TODO (yenda) remove once go implements persistence
                                                                (assoc :sym-key sym-key))}}
                         (message/receive message chat-id chat-id)))))
