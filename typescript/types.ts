/* @generated from adl module types */

import * as ADL from './runtime/adl';

export type FilePath = string;

const FilePath_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"FilePath","version":{"kind":"nothing"}}};

export function texprFilePath(): ADL.ATypeExpr<FilePath> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "types",name : "FilePath"}}, parameters : []}};
}

export type S3Path = string;

const S3Path_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"S3Path","version":{"kind":"nothing"}}};

export function texprS3Path(): ADL.ATypeExpr<S3Path> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "types",name : "S3Path"}}, parameters : []}};
}

export type EndPointLabel = string;

const EndPointLabel_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"EndPointLabel","version":{"kind":"nothing"}}};

export function texprEndPointLabel(): ADL.ATypeExpr<EndPointLabel> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "types",name : "EndPointLabel"}}, parameters : []}};
}

export type DeployLabel = string;

const DeployLabel_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"DeployLabel","version":{"kind":"nothing"}}};

export function texprDeployLabel(): ADL.ATypeExpr<DeployLabel> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "types",name : "DeployLabel"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "types.FilePath" : FilePath_AST,
  "types.S3Path" : S3Path_AST,
  "types.EndPointLabel" : EndPointLabel_AST,
  "types.DeployLabel" : DeployLabel_AST
};
