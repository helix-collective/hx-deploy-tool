/* @generated from adl module config */

import * as ADL from './runtime/adl';
import * as sys_types from './sys/types';
import * as types from './types';

/**
 * Configuration file for the deployment tool
 */
export interface ToolConfig {
  releasesDir: types.FilePath;
  contextCache: types.FilePath;
  logFile: types.FilePath;
  letsencryptPrefixDir: types.FilePath;
  letsencryptWwwDir: types.FilePath;
  /**
   * If the deploy tool needs to generate an SSL certificate
   * using letsencrypt, it will be called this.
   */
  autoCertName: string;
  autoCertContactEmail: string;
  /**
   * The storage location for release zip files
   */
  releases: BlobStoreConfig;
  /**
   * The storage location for deployment context files
   */
  deployContext: BlobStoreConfig;
  deployContextFiles: DeployContextFile[];
  deployMode: DeployMode;
}

export function makeToolConfig(
  input: {
    releasesDir?: types.FilePath,
    contextCache?: types.FilePath,
    logFile?: types.FilePath,
    letsencryptPrefixDir?: types.FilePath,
    letsencryptWwwDir?: types.FilePath,
    autoCertName?: string,
    autoCertContactEmail?: string,
    releases: BlobStoreConfig,
    deployContext: BlobStoreConfig,
    deployContextFiles: DeployContextFile[],
    deployMode?: DeployMode,
  }
): ToolConfig {
  return {
    releasesDir: input.releasesDir === undefined ? "/opt/releases" : input.releasesDir,
    contextCache: input.contextCache === undefined ? "/opt/etc/deployment" : input.contextCache,
    logFile: input.logFile === undefined ? "/opt/var/log/hx-deploy-tool.log" : input.logFile,
    letsencryptPrefixDir: input.letsencryptPrefixDir === undefined ? "/opt" : input.letsencryptPrefixDir,
    letsencryptWwwDir: input.letsencryptWwwDir === undefined ? "/opt/var/www" : input.letsencryptWwwDir,
    autoCertName: input.autoCertName === undefined ? "hxdeploytoolcert" : input.autoCertName,
    autoCertContactEmail: input.autoCertContactEmail === undefined ? "" : input.autoCertContactEmail,
    releases: input.releases,
    deployContext: input.deployContext,
    deployContextFiles: input.deployContextFiles,
    deployMode: input.deployMode === undefined ? {kind : "select"} : input.deployMode,
  };
}

const ToolConfig_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"releasesDir","default":{"kind":"just","value":"/opt/releases"},"name":"releasesDir","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"contextCache","default":{"kind":"just","value":"/opt/etc/deployment"},"name":"contextCache","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"logFile","default":{"kind":"just","value":"/opt/var/log/hx-deploy-tool.log"},"name":"logFile","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"letsencryptPrefixDir","default":{"kind":"just","value":"/opt"},"name":"letsencryptPrefixDir","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"letsencryptWwwDir","default":{"kind":"just","value":"/opt/var/www"},"name":"letsencryptWwwDir","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"autoCertName","default":{"kind":"just","value":"hxdeploytoolcert"},"name":"autoCertName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"autoCertContactEmail","default":{"kind":"just","value":""},"name":"autoCertContactEmail","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"releases","default":{"kind":"nothing"},"name":"releases","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"BlobStoreConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"deployContext","default":{"kind":"nothing"},"name":"deployContext","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"BlobStoreConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"deployContextFiles","default":{"kind":"nothing"},"name":"deployContextFiles","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"DeployContextFile"}},"parameters":[]}]}},{"annotations":[],"serializedName":"deployMode","default":{"kind":"just","value":"select"},"name":"deployMode","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"DeployMode"}},"parameters":[]}}]}},"name":"ToolConfig","version":{"kind":"nothing"}}};

export function texprToolConfig(): ADL.ATypeExpr<ToolConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "ToolConfig"}}, parameters : []}};
}

export interface DeployMode_Select {
  kind: 'select';
}
export interface DeployMode_Proxy {
  kind: 'proxy';
  value: ProxyModeConfig;
}

export type DeployMode = DeployMode_Select | DeployMode_Proxy;

const DeployMode_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"select","default":{"kind":"nothing"},"name":"select","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"proxy","default":{"kind":"nothing"},"name":"proxy","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"ProxyModeConfig"}},"parameters":[]}}]}},"name":"DeployMode","version":{"kind":"nothing"}}};

export function texprDeployMode(): ADL.ATypeExpr<DeployMode> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "DeployMode"}}, parameters : []}};
}

export interface BlobStoreConfig_S3 {
  kind: 's3';
  value: types.S3Path;
}
export interface BlobStoreConfig_Localdir {
  kind: 'localdir';
  value: types.FilePath;
}

export type BlobStoreConfig = BlobStoreConfig_S3 | BlobStoreConfig_Localdir;

const BlobStoreConfig_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"s3","default":{"kind":"nothing"},"name":"s3","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"S3Path"}},"parameters":[]}},{"annotations":[],"serializedName":"localdir","default":{"kind":"nothing"},"name":"localdir","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}}]}},"name":"BlobStoreConfig","version":{"kind":"nothing"}}};

export function texprBlobStoreConfig(): ADL.ATypeExpr<BlobStoreConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "BlobStoreConfig"}}, parameters : []}};
}

export interface ProxyModeConfig {
  /**
   * The configured endpoints.
   */
  endPoints: {[key: string]: EndPoint};
  /**
   * If set, we are in remote mode, with state stored at this S3 path
   */
  remoteStateS3: sys_types.Maybe<types.S3Path>;
  /**
   * When we start deploys we choose a port from this range
   */
  dynamicPortRange: sys_types.Pair<number, number>;
  /**
   * How we generate identifiers for slave machines
   */
  slaveLabel: MachineLabel;
}

export function makeProxyModeConfig(
  input: {
    endPoints: {[key: string]: EndPoint},
    remoteStateS3?: sys_types.Maybe<types.S3Path>,
    dynamicPortRange?: sys_types.Pair<number, number>,
    slaveLabel?: MachineLabel,
  }
): ProxyModeConfig {
  return {
    endPoints: input.endPoints,
    remoteStateS3: input.remoteStateS3 === undefined ? {kind : "nothing"} : input.remoteStateS3,
    dynamicPortRange: input.dynamicPortRange === undefined ? {v1 : 8000, v2 : 8100} : input.dynamicPortRange,
    slaveLabel: input.slaveLabel === undefined ? {kind : "ec2InstanceId"} : input.slaveLabel,
  };
}

const ProxyModeConfig_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"endPoints","default":{"kind":"nothing"},"name":"endPoints","typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"EndPoint"}},"parameters":[]}]}},{"annotations":[],"serializedName":"remoteStateS3","default":{"kind":"just","value":"nothing"},"name":"remoteStateS3","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"S3Path"}},"parameters":[]}]}},{"annotations":[],"serializedName":"dynamicPortRange","default":{"kind":"just","value":{"v1":8000,"v2":8100}},"name":"dynamicPortRange","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Pair"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Word32"},"parameters":[]},{"typeRef":{"kind":"primitive","value":"Word32"},"parameters":[]}]}},{"annotations":[],"serializedName":"slaveLabel","default":{"kind":"just","value":"ec2InstanceId"},"name":"slaveLabel","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"MachineLabel"}},"parameters":[]}}]}},"name":"ProxyModeConfig","version":{"kind":"nothing"}}};

export function texprProxyModeConfig(): ADL.ATypeExpr<ProxyModeConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "ProxyModeConfig"}}, parameters : []}};
}

export interface MachineLabel_Label {
  kind: 'label';
  value: string;
}
export interface MachineLabel_Ec2InstanceId {
  kind: 'ec2InstanceId';
}

export type MachineLabel = MachineLabel_Label | MachineLabel_Ec2InstanceId;

const MachineLabel_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"label","default":{"kind":"nothing"},"name":"label","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"ec2InstanceId","default":{"kind":"nothing"},"name":"ec2InstanceId","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"MachineLabel","version":{"kind":"nothing"}}};

export function texprMachineLabel(): ADL.ATypeExpr<MachineLabel> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "MachineLabel"}}, parameters : []}};
}

export interface EndPoint {
  label: types.EndPointLabel;
  serverName: string;
  etype: EndPointType;
}

export function makeEndPoint(
  input: {
    label: types.EndPointLabel,
    serverName: string,
    etype: EndPointType,
  }
): EndPoint {
  return {
    label: input.label,
    serverName: input.serverName,
    etype: input.etype,
  };
}

const EndPoint_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"label","default":{"kind":"nothing"},"name":"label","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"EndPointLabel"}},"parameters":[]}},{"annotations":[],"serializedName":"serverName","default":{"kind":"nothing"},"name":"serverName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"etype","default":{"kind":"nothing"},"name":"etype","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"EndPointType"}},"parameters":[]}}]}},"name":"EndPoint","version":{"kind":"nothing"}}};

export function texprEndPoint(): ADL.ATypeExpr<EndPoint> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "EndPoint"}}, parameters : []}};
}

export interface EndPointType_HttpOnly {
  kind: 'httpOnly';
}
export interface EndPointType_HttpsWithRedirect {
  kind: 'httpsWithRedirect';
  value: SslCertMode;
}

export type EndPointType = EndPointType_HttpOnly | EndPointType_HttpsWithRedirect;

const EndPointType_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"httpOnly","default":{"kind":"nothing"},"name":"httpOnly","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"httpsWithRedirect","default":{"kind":"nothing"},"name":"httpsWithRedirect","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"SslCertMode"}},"parameters":[]}}]}},"name":"EndPointType","version":{"kind":"nothing"}}};

export function texprEndPointType(): ADL.ATypeExpr<EndPointType> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "EndPointType"}}, parameters : []}};
}

export interface SslCertMode_Generated {
  kind: 'generated';
}
export interface SslCertMode_Explicit {
  kind: 'explicit';
  value: SslCertPaths;
}

export type SslCertMode = SslCertMode_Generated | SslCertMode_Explicit;

const SslCertMode_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"generated","default":{"kind":"nothing"},"name":"generated","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"explicit","default":{"kind":"nothing"},"name":"explicit","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"SslCertPaths"}},"parameters":[]}}]}},"name":"SslCertMode","version":{"kind":"nothing"}}};

export function texprSslCertMode(): ADL.ATypeExpr<SslCertMode> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "SslCertMode"}}, parameters : []}};
}

export interface SslCertPaths {
  sslCertificate: types.FilePath;
  sslCertificateKey: types.FilePath;
}

export function makeSslCertPaths(
  input: {
    sslCertificate: types.FilePath,
    sslCertificateKey: types.FilePath,
  }
): SslCertPaths {
  return {
    sslCertificate: input.sslCertificate,
    sslCertificateKey: input.sslCertificateKey,
  };
}

const SslCertPaths_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"sslCertificate","default":{"kind":"nothing"},"name":"sslCertificate","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"sslCertificateKey","default":{"kind":"nothing"},"name":"sslCertificateKey","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}}]}},"name":"SslCertPaths","version":{"kind":"nothing"}}};

export function texprSslCertPaths(): ADL.ATypeExpr<SslCertPaths> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "SslCertPaths"}}, parameters : []}};
}

export interface DeployContextFile {
  name: types.FilePath;
  sourceName: string;
}

export function makeDeployContextFile(
  input: {
    name: types.FilePath,
    sourceName: string,
  }
): DeployContextFile {
  return {
    name: input.name,
    sourceName: input.sourceName,
  };
}

const DeployContextFile_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"sourceName","default":{"kind":"nothing"},"name":"sourceName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"DeployContextFile","version":{"kind":"nothing"}}};

export function texprDeployContextFile(): ADL.ATypeExpr<DeployContextFile> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "DeployContextFile"}}, parameters : []}};
}

export enum Verbosity {
  quiet,
  noisy,
}

const Verbosity_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"quiet","default":{"kind":"nothing"},"name":"quiet","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"noisy","default":{"kind":"nothing"},"name":"noisy","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"Verbosity","version":{"kind":"nothing"}}};

export function texprVerbosity(): ADL.ATypeExpr<Verbosity> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "Verbosity"}}, parameters : []}};
}

/**
 * Configuration specification for the letsencrypt related functions
 */
export interface LetsEncryptConfig {
  /**
   * The path to the install certbot executable
   */
  certbotPath: string;
  /**
   * The ID of the AWS hosted zone containing the SSL DNS entries
   */
  awsHostedZoneId: string;
  /**
   * The directory within which certbot will it's working files
   * and live certificates
   */
  basedir: string;
  /**
   * The email address that certbot will use for essential communications
   */
  email: string;
  /**
   * The fully scoped DNS names required on the certificate
   */
  domains: string[];
  /**
   * How much logging output to generate
   */
  verbosity: Verbosity;
}

export function makeLetsEncryptConfig(
  input: {
    certbotPath: string,
    awsHostedZoneId: string,
    basedir: string,
    email: string,
    domains: string[],
    verbosity?: Verbosity,
  }
): LetsEncryptConfig {
  return {
    certbotPath: input.certbotPath,
    awsHostedZoneId: input.awsHostedZoneId,
    basedir: input.basedir,
    email: input.email,
    domains: input.domains,
    verbosity: input.verbosity === undefined ? 0 : input.verbosity,
  };
}

const LetsEncryptConfig_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"certbotPath","default":{"kind":"nothing"},"name":"certbotPath","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"awsHostedZoneId","default":{"kind":"nothing"},"name":"awsHostedZoneId","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"basedir","default":{"kind":"nothing"},"name":"basedir","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"domains","default":{"kind":"nothing"},"name":"domains","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"verbosity","default":{"kind":"just","value":"quiet"},"name":"verbosity","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"config","name":"Verbosity"}},"parameters":[]}}]}},"name":"LetsEncryptConfig","version":{"kind":"nothing"}}};

export function texprLetsEncryptConfig(): ADL.ATypeExpr<LetsEncryptConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "config",name : "LetsEncryptConfig"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "config.ToolConfig" : ToolConfig_AST,
  "config.DeployMode" : DeployMode_AST,
  "config.BlobStoreConfig" : BlobStoreConfig_AST,
  "config.ProxyModeConfig" : ProxyModeConfig_AST,
  "config.MachineLabel" : MachineLabel_AST,
  "config.EndPoint" : EndPoint_AST,
  "config.EndPointType" : EndPointType_AST,
  "config.SslCertMode" : SslCertMode_AST,
  "config.SslCertPaths" : SslCertPaths_AST,
  "config.DeployContextFile" : DeployContextFile_AST,
  "config.Verbosity" : Verbosity_AST,
  "config.LetsEncryptConfig" : LetsEncryptConfig_AST
};
