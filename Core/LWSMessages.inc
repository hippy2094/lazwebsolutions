{$DEFINE LSLANGUAGE_EN}
{.$DEFINE LSLANGUAGE_PTBR}
{$IFDEF LSLANGUAGE_EN}
  SLWSFileNotFound_rst = 'File not found: %s';
  SLWSLengthRequiredError_rst = 'ERROR: 411 - Length Required.';
  SLWSInternalServerError_rst = 'ERROR: 500 - Internal Server Error.';
  SLWSNoREQUEST_METHODPassedFromServerError_rst = 'ERROR: No REQUEST_METHOD passed from server.';
{$ENDIF}
{$IFDEF LSLANGUAGE_PTBR}
  SLWSFileNotFound_rst = 'Arquivo não encontrado: %s';
  SLWSLengthRequiredError_rst = 'ERRO: 411 - Comprimento necessário.';
  SLWSInternalServerError_rst = 'ERRO: 500 - Erro interno do Servidor.';
  SLWSNoREQUEST_METHODPassedFromServerError_rst = 'ERRO: REQUEST_METHOD não informado pelo servidor.';
{$ENDIF}
