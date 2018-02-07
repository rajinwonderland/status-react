(ns status-im.ui.screens.contacts.events
  (:require [re-frame.core :as re-frame]
            [status-im.utils.handlers :as handlers]
            [clojure.string :as s]
<<<<<<< HEAD
            [status-im.protocol.core :as protocol]
            [status-im.utils.contacts :as utils.contacts]
=======
>>>>>>> 12145c63... First working version of new protocol
            [status-im.utils.utils :refer [http-post]]
            [status-im.utils.random :as random]
            [taoensso.timbre :as log]
            [cljs.reader :refer [read-string]]
            [status-im.utils.js-resources :as js-res]
            [status-im.react-native.js-dependencies :as rn-dependencies]
            [status-im.js-dependencies :as dependencies]
            [status-im.i18n :refer [label]]
            [status-im.ui.screens.contacts.navigation]
            [status-im.ui.screens.navigation :as navigation]
            [status-im.ui.screens.discover.events :as discover-events]
            [status-im.chat.console :as console-chat]
            [status-im.chat.events :as chat.events]
            [status-im.commands.events.loading :as loading-events]
            [status-im.transport.message.core :as transport]
            [status-im.transport.message.v1.contact :as transport-contact]
            [cljs.spec.alpha :as spec]
            [status-im.ui.screens.add-new.new-chat.db :as new-chat.db]
            [clojure.string :as string]
            [status-im.utils.datetime :as datetime]))
;;;; COFX

(re-frame/reg-cofx
  ::get-default-contacts-and-groups
  (fn [coeffects _]
<<<<<<< HEAD
    (assoc coeffects :all-contacts (contacts/get-all))))

(reg-cofx
 ::get-default-contacts-and-groups
 (fn [coeffects _]
   (assoc coeffects
          :default-contacts js-res/default-contacts
          :default-groups js-res/default-contact-groups)))

;;;; FX

(reg-fx
  ::watch-contact
  (fn [{:keys [web3 whisper-identity public-key private-key]}]
    (protocol/watch-user! {:web3     web3
                           :identity whisper-identity
                           :keypair  {:public  public-key
                                      :private private-key}
                           :callback #(dispatch [:incoming-message %1 %2])})))

(reg-fx
  ::stop-watching-contact
  (fn [{:keys [web3 whisper-identity]}]
    (protocol/stop-watching-user! {:web3     web3
                                   :identity whisper-identity})))

(reg-fx
  ::send-contact-request-fx
  (fn [{:keys [web3 current-public-key name whisper-identity
               photo-path current-account-id status fcm-token
               updates-public-key updates-private-key] :as params}]
    (protocol/contact-request!
     {:web3    web3
      :message {:from       current-public-key
                :to         whisper-identity
                :message-id (random/id)
                :payload    {:contact {:name          name
                                       :profile-image photo-path
                                       :address       current-account-id
                                       :status        status
                                       :fcm-token     fcm-token}
                             :keypair {:public  updates-public-key
                                       :private updates-private-key}
                             :timestamp (datetime/timestamp)}}})))

(reg-fx
  ::reset-pending-messages
  (fn [from]
    (protocol/reset-pending-messages! from)))

(reg-fx
  ::save-contact
  (fn [contact]
    (contacts/save contact)))

(reg-fx
  :save-all-contacts
  (fn [new-contacts]
    (contacts/save-all new-contacts)))

(reg-fx
  ::delete-contact
  (fn [contact]
    (contacts/delete contact)))
=======
    (assoc coeffects
           :default-contacts js-res/default-contacts
           :default-groups js-res/default-contact-groups)))
>>>>>>> 12145c63... First working version of new protocol

(defn- contact-name [contact]
  (->> contact
       ((juxt :givenName :middleName :familyName))
       (remove s/blank?)
       (s/join " ")))

;;;; Handlers

(handlers/register-handler-fx
  :update-contact!
  (fn [{:keys [db]} [_ {:keys [whisper-identity] :as contact}]]
    (when (get-in db [:contacts/contacts whisper-identity])
      {:db           (update-in db [:contacts/contacts whisper-identity] merge contact)
       :save-contact contact})))

(defn- update-pending-status [old-contacts {:keys [whisper-identity pending?] :as contact}]
  (let [{old-pending :pending?
         :as         old-contact} (get old-contacts whisper-identity)
        pending?' (if old-contact (and old-pending pending?) pending?)]
    (assoc contact :pending? (boolean pending?'))))

(defn- public-key->address [public-key]
  (let [length (count public-key)
        normalized-key (case length
                         132 (subs public-key 4)
                         130 (subs public-key 2)
                         128 public-key
                         nil)]
    (when normalized-key
      (subs (.sha3 dependencies/Web3.prototype normalized-key #js {:encoding "hex"}) 26))))

(defn- prepare-default-groups-events [groups default-groups]
  [[:add-contact-groups
    (for [[id {:keys [name contacts]}] default-groups
          :let [id' (clojure.core/name id)]
          :when (not (get groups id'))]
      {:group-id  id'
       :name      (:en name)
       :order     0
       :timestamp (datetime/timestamp)
       :contacts  (mapv #(hash-map :identity %) contacts)})]])

;; NOTE(oskarth): We now overwrite default contacts upon upgrade with default_contacts.json data.
(defn- prepare-default-contacts-events [contacts default-contacts]
  (let [default-contacts
        (for [[id {:keys [name photo-path public-key add-chat? pending? description
                          dapp? dapp-url dapp-hash bot-url unremovable? hide-contact?]}] default-contacts
              :let [id' (clojure.core/name id)]]
          {:whisper-identity id'
           :address          (public-key->address id')
           :name             (:en name)
           :photo-path       photo-path
           :public-key       public-key
           :unremovable?     (boolean unremovable?)
           :hide-contact?    (boolean hide-contact?)
           :pending?         pending?
           :dapp?            dapp?
           :dapp-url         (:en dapp-url)
           :bot-url          bot-url
           :description      description
           :dapp-hash        dapp-hash})
        all-default-contacts (conj default-contacts console-chat/contact)]
    [[:add-contacts all-default-contacts]]))

(defn- prepare-add-chat-events [contacts default-contacts]
  (for [[id {:keys [name add-chat?]}] default-contacts
        :let [id' (clojure.core/name id)]
        :when (and (not (get contacts id')) add-chat?)]
    [:add-chat id' {:name (:en name)}]))

(defn- prepare-add-contacts-to-groups-events [contacts default-contacts]
  (let [groups (for [[id {:keys [groups]}] default-contacts
                     :let [id' (clojure.core/name id)]
                     :when (and (not (get contacts id')) groups)]
                 (for [group groups]
                   {:group-id group :whisper-identity id'}))
        groups' (vals (group-by :group-id (flatten groups)))]
    (for [contacts groups']
      [:add-contacts-to-group
       (:group-id (first contacts))
       (mapv :whisper-identity contacts)])))

(handlers/register-handler-fx
  :load-default-contacts!
  [(re-frame/inject-cofx ::get-default-contacts-and-groups)]
  (fn [{:keys [db default-contacts default-groups]} _]
    (let [{:contacts/keys [contacts] :group/keys [contact-groups]} db]
      {:dispatch-n (concat
                    (prepare-default-groups-events contact-groups default-groups)
                    (prepare-default-contacts-events contacts default-contacts)
                    (prepare-add-chat-events contacts default-contacts)
                    (prepare-add-contacts-to-groups-events contacts default-contacts))})))

(handlers/register-handler-fx
  :load-contacts
  [(re-frame/inject-cofx :get-all-contacts)]
  (fn [{:keys [db all-contacts]} _]
    (let [contacts-list (map #(vector (:whisper-identity %) %) all-contacts)
          contacts (into {} contacts-list)]
      {:db (update db :contacts/contacts #(merge contacts %))})))

(handlers/register-handler-fx
  :add-contacts
  [(re-frame/inject-cofx :get-local-storage-data)]
  (fn [{:keys [db] :as cofx} [_ new-contacts]]
    (let [{:contacts/keys [contacts]} db
          new-contacts' (->> new-contacts
                             (map #(update-pending-status contacts %))
                             ;; NOTE(oskarth): Overwriting default contacts here
                             ;;(remove #(identities (:whisper-identity %)))
                             (map #(vector (:whisper-identity %) %))
                             (into {}))
          fx            {:db            (update db :contacts/contacts merge new-contacts')
                         :save-contacts (vals new-contacts')}]
      (transduce (map second)
                 (completing (partial loading-events/load-commands (assoc cofx :db (:db fx))))
                 fx
                 new-contacts'))))

(defn- add-new-contact [{:keys [whisper-identity] :as contact} {:keys [db]}]
  {:db          (-> db
                    (update-in [:contacts/contacts whisper-identity] merge contact)
                    (assoc-in [:contacts/new-identity] ""))
   :save-contact contact})

(defn- own-info [{:accounts/keys [accounts current-account-id] :as db}]
  (let [{:keys [name photo-path address]} (get accounts current-account-id)
        fcm-token (get-in db [:notifications :fcm-token])]
    {:name          name
     :profile-image photo-path
     :address       address
     :fcm-token     fcm-token}))

(defn- send-contact-request [{:keys [whisper-identity dapp?] :as contact} {:keys [db] :as cofx}]
  (when-not dapp?
    (transport/send (transport-contact/map->ContactRequest (own-info db)) whisper-identity cofx)))

<<<<<<< HEAD
(defn add-contact [{:keys [db] :as fx} chat-or-whisper-id]
  (let [{:keys [chats] :contacts/keys [contacts]} db
        contact (if-let [contact-info (get-in chats [chat-or-whisper-id :contact-info])]
                  (read-string contact-info)
                  (or (get contacts chat-or-whisper-id)
                      (utils.contacts/whisper-id->new-contact chat-or-whisper-id)))
        contact' (assoc contact
                        :address (public-key->address chat-or-whisper-id)
                        :pending? false)]
    (-> fx
        (watch-contact contact')
        (discover-events/send-portions-when-contact-exists chat-or-whisper-id)
        (add-new-contact contact'))))

(defn add-contact-and-open-chat [fx whisper-id]
  (-> fx
      (add-contact whisper-id)
      (update :db #(navigation/navigate-to-clean % :home))
      (assoc :dispatch [:start-chat whisper-id {:navigation-replace? true}])))

(register-handler-fx
  :add-contact
  (fn [{:keys [db]} [_ chat-or-whisper-id]]
    (add-contact {:db db} chat-or-whisper-id)))

(register-handler-fx
  :add-contact-and-open-chat
  (fn [{:keys [db]} [_ whisper-id]]
    (add-contact-and-open-chat {:db db} whisper-id)))
=======
(defn- send-contact-request-confirmation [{:keys [whisper-identity] :as contact} {:keys [db] :as cofx}]
  (transport/send (transport-contact/map->ContactRequestConfirmed (own-info db)) whisper-identity cofx))

(handlers/register-handler-fx
  :add-new-contact-and-open-chat
  (fn [{:keys [db] :as cofx} [_ {:keys [whisper-identity] :as contact}]]
    (when-not (get-in db [:contacts/contacts whisper-identity])
      (let [contact (assoc contact :address (public-key->address whisper-identity))]
        (handlers/merge-fx cofx
                           (navigation/navigate-to-clean :home)
                           (add-new-contact contact)
                           (chat.events/start-chat whisper-identity {:navigation-replace? true})
                           (send-contact-request contact))))))

(defn add-pending-contact [chat-or-whisper-id {:keys [db] :as cofx}]
  (let [{:keys [chats] :contacts/keys [contacts]} db
        contact (-> (if-let [contact-info (get-in chats [chat-or-whisper-id :contact-info])]
                      (read-string contact-info)
                      (get contacts chat-or-whisper-id))
                    (assoc :address  (public-key->address chat-or-whisper-id)
                           :pending? false))]
    (handlers/merge-fx cofx
                       (add-new-contact contact)
                       (send-contact-request-confirmation contact))))

(handlers/register-handler-fx
  :add-pending-contact
  (fn [cofx [_ chat-or-whisper-id]]
    (add-pending-contact chat-or-whisper-id cofx)))

(handlers/register-handler-fx
  :add-pending-contact-and-open-chat
  (fn [cofx [_ whisper-id]]
    (handlers/merge-fx cofx
                       (navigation/navigate-to-clean :home)
                       (add-pending-contact whisper-id)
                       (chat.events/start-chat whisper-id {:navigation-replace? true}))))
>>>>>>> 12145c63... First working version of new protocol

(handlers/register-handler-fx
  :set-contact-identity-from-qr
  (fn [{{:accounts/keys [accounts current-account-id] :as db} :db} [_ _ contact-identity]]
    (let [current-account (get accounts current-account-id)]
      (cond-> {:db (assoc db :contacts/new-identity contact-identity)}
        (not (new-chat.db/validate-pub-key contact-identity current-account))
        (assoc :dispatch [:add-contact-handler])))))

(handlers/register-handler-fx
  :contact-update-received
  (fn [{:keys [db]} [_ {:keys [from payload]}]]
    (let [{:keys [chats current-public-key]} db]
      (when (not= current-public-key from)
        (let [{:keys [content timestamp]} payload
              {:keys [status name profile-image]} (:profile content)
              prev-last-updated (get-in db [:contacts/contacts from :last-updated])]
          (when (<= prev-last-updated timestamp)
            (let [contact {:whisper-identity from
                           :name             name
                           :photo-path       profile-image
                           :status           status
                           :last-updated     timestamp}]
              {:dispatch-n (concat [[:update-contact! contact]]
                                   (when (chats from)
                                     [[:update-chat! {:chat-id from
                                                      :name    name}]]))})))))))

(handlers/register-handler-fx
  :update-keys-received
  (fn [{:keys [db]} [_ {:keys [from payload]}]]
    (let [{{:keys [public private]} :keypair
           timestamp                :timestamp} payload
          prev-last-updated (get-in db [:contacts/contacts from :keys-last-updated])]
      (when (<= prev-last-updated timestamp)
        (let [contact {:whisper-identity  from
                       :public-key        public
                       :private-key       private
                       :keys-last-updated timestamp}]
          {:dispatch [:update-contact! contact]})))))

(handlers/register-handler-fx
  :contact-online-received
  (fn [{:keys [db]} [_ {:keys                          [from]
                        {{:keys [timestamp]} :content} :payload}]]
    (let [prev-last-online (get-in db [:contacts/contacts from :last-online])]
      (when (and timestamp (< prev-last-online timestamp))
        {::reset-pending-messages from
         :dispatch                [:update-contact! {:whisper-identity from
                                                     :last-online      timestamp}]}))))

(handlers/register-handler-fx
  :hide-contact
  (fn [{:keys [db]} [_ {:keys [whisper-identity] :as contact}]]
    {::stop-watching-contact (merge
                              (select-keys db [:web3])
                              (select-keys contact [:whisper-identity]))
     :dispatch-n [[:update-contact! {:whisper-identity whisper-identity
                                     :pending?         true}]
                  [:account-update-keys]]}))

;;used only by status-dev-cli
(handlers/register-handler-fx
  :remove-contact
  (fn [{:keys [db]} [_ whisper-identity pred]]
    (let [contact (get-in db [:contacts/contacts whisper-identity])]
      (when (and contact (pred contact))
        {:db             (update db :contacts/contacts dissoc whisper-identity)
         :delete-contact contact}))))

(handlers/register-handler-fx
  :open-contact-toggle-list
  (fn [{:keys [db]} [_ group-type]]
    {:db       (-> db
                   (assoc :group/group-type group-type
                          :group/selected-contacts #{}
                          :new-chat-name "")
                   (navigation/navigate-to :contact-toggle-list))}))

(handlers/register-handler-fx
  :open-chat-with-contact
  (fn [{:keys [db] :as cofx} [_ {:keys [whisper-identity] :as contact}]]
    (handlers/merge-fx cofx
                       (navigation/navigate-to-clean :home)
                       (chat.events/start-chat whisper-identity)
                       (send-contact-request contact))))

(handlers/register-handler-fx
  :add-contact-handler
  (fn [{{:contacts/keys [new-identity] :as db} :db}]
    (when (seq new-identity)
      (add-contact-and-open-chat {:db db} new-identity))))
