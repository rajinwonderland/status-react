(ns status-im.transport.message.v1.group-chat
  (:require [re-frame.core :as re-frame]
            [status-im.utils.handlers :as handlers]
            [status-im.transport.message.core :as message]
            [status-im.transport.message.v1.protocol :as protocol]
            [status-im.transport.utils :as transport.utils]))

(defrecord NewGroupKey [sym-key message]
  message/StatusMessage
  (send [this chat-id cofx]
    (protocol/send-with-pubkey {:chat-id chat-id
                                :payload this}
                               cofx))
  (receive [this chat-id signature cofx]
    (handlers/merge-fx cofx
                       {:shh/add-new-sym-key {:web3 (get-in cofx [:db :web3])
                                              :sym-key sym-key
                                              :chat-id chat-id
                                              :message message
                                              :success-event ::add-new-sym-key}}
                       (protocol/init-chat chat-id))))

(defrecord GroupAdminAdd [added-participants]
  message/StatusMessage
  (send [this chat-id cofx])
  (receive [this chat-id signature cofx]))

(defrecord GroupAdminRemove [removed-participants]
  message/StatusMessage
  (send [this chat-id cofx])
  (receive [this chat-id signature cofx]))

(defrecord GroupInvite [content content-type message-type to-clock-value timestamp]
  message/StatusMessage
  (send [this chat-id cofx])
  (receive [this chat-id signature cofx]))

(defrecord GroupLeave []
  message/StatusMessage
  (send [this chat-id cofx])
  (receive [this chat-id signature cofx]))

(handlers/register-handler-fx
  ::send-new-sym-key
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
                         (message/send (NewGroupKey. sym-key message)
                                       chat-id)))))

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
