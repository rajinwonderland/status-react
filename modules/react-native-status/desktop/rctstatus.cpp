/**
 * Copyright (c) 2017-present, Status Research and Development GmbH.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

#include "rctstatus.h"
#include "bridge.h"
#include "eventdispatcher.h"

#include <QDebug>
#include <QJsonDocument>
#include <QByteArray>
#include <QVariantMap>

#include "libstatus.h"

namespace {
struct RegisterQMLMetaType {
    RegisterQMLMetaType() {
        qRegisterMetaType<RCTStatus*>();
    }
} registerMetaType;
} // namespace

class RCTStatusPrivate {
public:
    Bridge* bridge = nullptr;
};

RCTStatus::RCTStatus(QObject* parent) : QObject(parent), d_ptr(new RCTStatusPrivate) {}

RCTStatus::~RCTStatus() {}

void RCTStatus::setBridge(Bridge* bridge) {
    Q_D(RCTStatus);
    d->bridge = bridge;
}

QString RCTStatus::moduleName() {
    return "Status";
}

QList<ModuleMethod*> RCTStatus::methodsToExport() {
    return QList<ModuleMethod*>{};
}

QVariantMap RCTStatus::constantsToExport() {
    return QVariantMap();
}

void RCTStatus::initJail(QString js, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::initJail with param js:" << " and callback id: " << callbackId;

    InitJail(js.toUtf8().data());

    d->bridge->invokePromiseCallback(callbackId, QVariantList{ "{\"result\":\"\"}" });
}


void RCTStatus::parseJail(QString chatId, QString js, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::parseJail with param chatId: " << chatId << " js:" << " and callback id: " << callbackId;

    const char* result = Parse(chatId.toUtf8().data(), js.toUtf8().data());
    qDebug() << "RCTStatus::parseJail parseJail result: " << result;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{result});
}


void RCTStatus::callJail(QString chatId, QString path, QString params, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::callJail with param chatId: " << chatId << " path: " << path << " params: " << params <<  " and callback id: " << callbackId;

    const char* result = Call(chatId.toUtf8().data(), path.toUtf8().data(), params.toUtf8().data());
    qDebug() << "RCTStatus::callJail callJail result: " << result;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{result});
}


void RCTStatus::startNode(QString configString) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::startNode with param configString:" << configString;

    QJsonParseError jsonError;
    QJsonDocument jsonDoc = QJsonDocument::fromJson(configString.toUtf8(), &jsonError);
    if (jsonError.error != QJsonParseError::NoError){
        qDebug() << jsonError.errorString();
    }

    qDebug() << " RCTStatus::startNode configString: " << jsonDoc.toVariant().toMap();
    QVariantMap configJSON = jsonDoc.toVariant().toMap();

    QString newKeystoreUrl = "keystore";

    int networkId = configJSON["NetworkId"].toInt();
    QString dataDir = configJSON["DataDir"].toString();

    QString networkDir = "./" + dataDir;
    int dev = 0;

    char *configChars = GenerateConfig(networkDir.toUtf8().data(), networkId, dev);
    qDebug() << "RCTStatus::startNode GenerateConfig result: " << configChars;

    jsonDoc = QJsonDocument::fromJson(QString(configChars).toUtf8(), &jsonError);
    if (jsonError.error != QJsonParseError::NoError){
        qDebug() << jsonError.errorString();
    }

    qDebug() << " RCTStatus::startNode GenerateConfig configString: " << jsonDoc.toVariant().toMap();
    QVariantMap generatedConfig = jsonDoc.toVariant().toMap();
    generatedConfig["KeyStoreDir"] = newKeystoreUrl;
    generatedConfig["LogEnabled"] = "1";
    generatedConfig["LogFile"] = networkDir + "/geth.log";
    //generatedConfig["LogLevel"] = "DEBUG";

    const char* result = StartNode(QString(QJsonDocument::fromVariant(generatedConfig).toJson(QJsonDocument::Compact)).toUtf8().data());
    qDebug() << "RCTStatus::startNode StartNode result: " << result;

    d->bridge->eventDispatcher()->sendDeviceEvent("gethEvent", QVariantMap{{"jsonEvent", "{\"type\":\"node.started\"}"}});
    d->bridge->eventDispatcher()->sendDeviceEvent("gethEvent", QVariantMap{{"jsonEvent", "{\"type\":\"node.ready\"}"}});
}


void RCTStatus::shouldMoveToInternalStorage(double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::shouldMoveToInternalStorage with param callbackId: " << callbackId;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{ "{\"result\":\"\"}" });
}


void RCTStatus::moveToInternalStorage(double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::moveToInternalStorage with param callbackId: " << callbackId;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{ "{\"result\":\"\"}" });
}


void RCTStatus::stopNode() {
    qDebug() << "call of RCTStatus::stopNode";
    const char* result = StopNode();
    qDebug() << "RCTStatus::stopNode StopNode result: " << result;
}


void RCTStatus::createAccount(QString password, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::createAccount with param callbackId: " << callbackId;
    const char* result = CreateAccount(password.toUtf8().data());
    qDebug() << "RCTStatus::createAccount CreateAccount result: " << result;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{result});
}


void RCTStatus::notify(QString token, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::notify with param callbackId: " << callbackId;
    const char* result = Notify(token.toUtf8().data());
    qDebug() << "RCTStatus::notify Notify result: " << result;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{result});
}


void RCTStatus::addPeer(QString enode, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::addPeer with param callbackId: " << callbackId;
    const char* result = AddPeer(enode.toUtf8().data());
    qDebug() << "RCTStatus::addPeer AddPeer result: " << result;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{result});
}


void RCTStatus::recoverAccount(QString passphrase, QString password, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::recoverAccount with param callbackId: " << callbackId;
    const char* result = RecoverAccount(password.toUtf8().data(), passphrase.toUtf8().data());
    qDebug() << "RCTStatus::recoverAccount RecoverAccount result: " << result;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{result});
}


void RCTStatus::login(QString address, QString password, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::login with param callbackId: " << callbackId;
    const char* result = Login(address.toUtf8().data(), password.toUtf8().data());
    qDebug() << "RCTStatus::login Login result: " << result;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{result});
}


void RCTStatus::completeTransactions(QString hashes, QString password, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::completeTransactions with param callbackId: " << callbackId;
    const char* result = CompleteTransactions(hashes.toUtf8().data(), password.toUtf8().data());
    qDebug() << "RCTStatus::completeTransactions CompleteTransactions result: " << result;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{result});
}

void RCTStatus::discardTransaction(QString id) {
    qDebug() << "call of RCTStatus::discardTransaction with id: " << id;
    DiscardTransaction(id.toUtf8().data());
}

void RCTStatus::setAdjustResize() {
}


void RCTStatus::setAdjustPan() {
}


void RCTStatus::setSoftInputMode(int i) {
}



void RCTStatus::clearCookies() {
}


void RCTStatus::clearStorageAPIs() {
}


void RCTStatus::sendWeb3Request(QString payload, double callbackId) {
    Q_D(RCTStatus);
    qDebug() << "call of RCTStatus::sendWeb3Request with param callbackId: " << callbackId;
    d->bridge->invokePromiseCallback(callbackId, QVariantList{ "{\"result\":\"\"}" });
}


void RCTStatus::closeApplication() {
}

bool RCTStatus::JSCEnabled()
{
    qDebug() << "call of RCTStatus::JSCEnabled";
    return false;
}

void RCTStatus::signalEvent(const char* signal)
{
    qDebug() << "call of RCTStatus::signalEvent ... signal: " << signal;

}

void RCTStatus::jailEvent(QString chatId, QString data)
{
    qDebug() << "call of RCTStatus::jailEvent ... chatId: " << chatId << " data: " << data;
}


