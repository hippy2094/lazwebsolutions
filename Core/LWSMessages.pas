(*
  LazWebSolutions, Messages unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  http://code.google.com/p/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSMessages;

{$I lazwebsolutions.inc}

interface

resourcestring
  {$I LWSMessages.inc}

var
  // General msgs
  SLWSFileNotFound: string = SLWSFileNotFound_rst;
  SLWSInternalServerError: string = SLWSInternalServerError_rst;
  SLWSLengthRequiredError: string = SLWSLengthRequiredError_rst;
  SLWSNoREQUEST_METHODPassedFromServerError: string = SLWSNoREQUEST_METHODPassedFromServerError_rst;

implementation

end.

