(*
  LazWebSolutions, JSON Data Objects unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSJSONDataObjectsConsts;

{$I lazwebsolutions.inc}

interface

const
  AK = '*';
  CO = ':';
  CS = ', ';
  BS = '[ ';
  BE = ' ]';
  EQ = ' = ';
  ES = '';
  PE = ')';
  PS = '(';
  PO = '#';
  SP = ' ';
  DEFAULT_PK_FIELD = 'id';
  CONNECTOR_NAME = 'connectorname';
  SQL_SELECT_TOKEN = 'select ';
  SQL_FROM_TOKEN = ' from ';
  SQL_WHERE_TOKEN = ' where ';
  SQL_ORDER_BY_TOKEN = ' order by ';
  SQL_INSERT_TOKEN = 'insert into ';
  SQL_VALUES_TOKEN = ' values ';
  SQL_UPDATE_TOKEN = 'update ';
  SQL_SET_TOKEN = ' set ';
  SQL_DELETE_TOKEN = 'delete';
  SQL_EQ_PARAM_TOKEN = ' = :';
  LWS_FT_NULL = 'null';
  LWS_FT_STR = 'str';
  LWS_FT_BOOL = 'bool';
  LWS_FT_DATE = 'date';
  LWS_FT_FLOAT = 'float';
  LWS_FT_INT = 'int';

var
  SLWSPKFieldNameEmptyError: string = '"PKFieldName" must not be empty.';
  SLWSJSONObjectToParamsError: string = '"AJSONFiels.Count" may not be different from the "AJSONObject.Count".';
  SLWSConnectorNameEmptyError: string = '"ConnectorName" must not be empty.';
  SLWSConnectorUnitWasNotDeclaredError: string = 'The unit of "%s" was not declared in uses clause.';
  SLWSConfigFileNotFoundError: string = 'Config file not found: %s';
  SLWSInvalidPropInConfigFile: string = 'Invalid property in "%s" file: %s';

implementation

end.

