{---------------------------------------------------------------------------------
This file is part of OpenARCK Project.

   Copyright (C) 2021  Everton Teles
   email: info@evertonteles.com
   home: www.evertonteles.com

OpenArch is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
----------------------------------------------------------------------------------

// History:
   2016 JUL 29 - Adjust of the function InterpretParam, now All is ok.
   2016 JUN 22 - move the TArckFamily to arck_core unit.
   2016 MAY 23 - Definição Algoritimo de leitura das sections, families, lists e Objects do sistema
   2016 MAY 18 - Revisao do Modelo de Classes
   2016 MAY 05 - Desenvolvimento código das Classes do pacote arck_system.pas
   2016 MAY 10 - 1 Versão do Modelo de CLasses
   2015 SEP 25 - Inicio Elaboração da Modelagem de Classes.
   2015 JUL 28 - Estudo de Classes
   2015 JUL 01 - Início de Implementação
}

 
unit OpenArchIOFile;

{$mode objfpc}{$H+}

interface 
 
uses  
  // FreePascal
  Classes, Dialogs, Contnrs, Graphics, strutils, DOM, XMLWrite,
  XMLRead, SysUtils, TypInfo,

 
  // GLScene 
  GLScene,
  GLViewer,
 
  // OpenARCH
  OpenArchIOFileA,
  OpenArchVectorGeometry,
  OpenArchSysParameters;
 
type  
{ ------------------------------------------------------------------------
                         TArchFileHeader
------------------------------------------------------------------------- }
  TArchFileHeader = class
    private
      FAuthor: string;
      FCreationDate: TDateTime;
      FCreationSystem: string;
      FFileName: string;
      FLastModificationSystem: string;
      FModificationDate: TDateTime;
      FOpenArchVersion: Double;
      procedure SetAuthor(AValue: string);
      procedure SetCreationDate(AValue: TDateTime);
      procedure SetCreationSystem(AValue: string);
      procedure SetFileName(AValue: string);
      procedure SetLastModificationSystem(AValue: string);
      procedure SetModificationDate(AValue: TDateTime);
      procedure SetOpenArchVersion(AValue: Double);
    public
      procedure LoadHeader(AParameters: TArchParamList);
      property FileName:string read FFileName write SetFileName;
      property Author:string read FAuthor write SetAuthor;
      property CreationDate:TDateTime read FCreationDate write SetCreationDate;
      property ModificationDate:TDateTime read FModificationDate write SetModificationDate;
      property CreationSystem:string read FCreationSystem write SetCreationSystem;
      property LastModificationSystem:string read FLastModificationSystem write SetLastModificationSystem;
      property OpenArchVersion:Double read FOpenArchVersion write SetOpenArchVersion;
  end;

{ ------------------------------------------------------------------------
                         TArchStreamReader
------------------------------------------------------------------------- }

{ TArchStreamReader }

TArchStreamReader = class(TInterfacedObject, IArchReader)
 private
  FStream: TStream;
  function ReadChar: Char; //Function to read char
  function ReadLn: String; //function to read a line string
 public
   constructor Create(const AStream : TStream);
   class function New(const AStream : TStream): TArchStreamReader;
   //Verifica se é inicio de uma section
   function CheckData(const ALine: String):string; Virtual;
   //Verifica se o parametro indica o fim de uma parte do arquivo
   function EndOfEntity(Param,Value: String): boolean;
   function EndOSession(Param,Value: String): boolean;
   procedure ReadParameters(var Param,Value: String);
   // Go to the specific session:
   function GoToSession(ASession: string):boolean;
   //Lê um ponto 3D
   function Read3DPoint(const Value:String): IArch3DPoint;
   // Read and get a StringList;
   function ReadStringList(const AValue:String): TStringList;
   property ArchStream:TStream read FStream write FStream;
end;
 
{ ------------------------------------------------------------------------
                         TArchStreamWriter
------------------------------------------------------------------------- }
TArchStreamWriter = class(TInterfacedObject, IArchWriter)
  private
    FStream : TStream;
    procedure WriteLn(Line: string); // metodo pra escrever uma linha
  public
    constructor Create(const AStream : TStream);
    class function New(const AStream:TStream): TArchStreamWriter;
    //Escrever parametros vindos do sistema no arquivo
    procedure WriteParam(Param,Value: String);
    //Escreve Um Ponto 3D
    procedure Write3DPoint(A3DPoint: IArch3DPoint; ABaseParam: String);
    procedure WriteSection(ASection:String);
    Procedure WriteObject(AId:String);
    Procedure WriteEndObject();
    property ArchStream:TStream read FStream write FStream;
  end;
 
{ ------------------------------------------------------------------------
                         TArchIOStreamFile
------------------------------------------------------------------------- }

{ TArchIOStreamFile }

TArchIOStreamFile = class(TInterfacedObject, IArchIOFile)
  private
    FReader: TArchStreamReader;
    FWriter: TArchStreamWriter;
    FHeader: TArchFileHeader;
    function GetParamList(Sender:TObject):TArchParamList;
    //function GetValue(AKind :
  public
    constructor Create(AStream: TStream);
    destructor Destroy;
    procedure SaveData(AParams: TArchParamList);
    function LoadData():TArchParamList;
    procedure LoadHeader();
    function LoadSection(ASection:String):TArchParamList;
    class function New(AStream: TStream):TArchIOStreamFile;
    property Reader: TArchStreamReader read FReader;
    property Writer: TArchStreamWriter read FWriter;
end;

{ ------------------------------------------------------------------------
                         TArchXMLReader
------------------------------------------------------------------------- }
TArchXMLReader = class(TInterfacedObject, IArchReader)
 private
  FXML: TXMLDocument;
  FRootNode: TDOMNode;
 public
   constructor Create(const AXML : TXMLDocument);
   class function New(const AXML : TXMLDocument): TArchXMLReader;
   //Verifica se o parametro indica o fim de uma parte do arquivo
   function EndOfEntity(ANode:TDOMNode): boolean;
   procedure ReadParameters(var Param,Value: String);
   //Lê um ponto 3D
   function Read3DPoint(const Value:String): IArch3DPoint;
   property ArchXML: TXMLDocument read FXML;
   property RootNode: TDOMNode read FRootNode write FRootNode;
end;

{ ------------------------------------------------------------------------
                         TArchXMLWriter
------------------------------------------------------------------------- }
TArchXMLWriter = class(TInterfacedObject, IArchWriter)
  private
    FXML : TXMLDocument;
    FRootNode: TDOMNode;
  public
    constructor Create(AXML : TXMLDocument);
    class function New(AXML : TXMLDocument): TArchXMLWriter;
    //Escrever parametros vindos do sistema no arquivo
    procedure WriteParam(Param,Value: String);
    //Escreve Um Ponto 3D
    procedure Write3DPoint(A3DPoint: IArch3DPoint; ABaseParam: String);
    procedure WriteSection(ASection:String);
    Procedure WriteObject(AId:String);
    Procedure WriteEndObject();
    property ArchXML: TXMLDocument read FXML;
    property RootNode: TDOMNode read FRootNode write FRootNode;
  end;

{ ------------------------------------------------------------------------
                         TArchIOXMLFile
------------------------------------------------------------------------- }
TArchIOXMLFile = class(TInterfacedObject, IArchIOFile)
  private
    FXML : TXMLDocument;
    FReader: TArchXMLReader;
    FWriter: TArchXMLWriter;
  public
    procedure SaveData(AParams: TArchParamList);
    function LoadData:TArchParamList;
    constructor Create(AXMLPath: String);
    destructor Destroy;
    class function New(AXMLPath: String): TArchIOXMLFile;
    property Writer: TArchXMLWriter read FWriter;
    property Reader: TArchXMLReader read FReader;
end;

//------------
implementation 
//------------
 
uses
 { FreePascal:}
 LCLType;

const
  cCR = $0D;
  cLF = $0A;
  cEOF = $1A;

{ ------------------------------------------------------------------------
                         TArchFileHeader
------------------------------------------------------------------------- }
procedure TArchFileHeader.SetFileName(AValue: string);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

procedure TArchFileHeader.SetLastModificationSystem(AValue: string);
begin
  if FLastModificationSystem=AValue then Exit;
  FLastModificationSystem:=AValue;
end;

procedure TArchFileHeader.SetModificationDate(AValue: TDateTime);
begin
  if FModificationDate=AValue then Exit;
  FModificationDate:=AValue;
end;

procedure TArchFileHeader.SetOpenArchVersion(AValue: Double);
begin
  if FOpenArchVersion=AValue then Exit;
  FOpenArchVersion:=AValue;
end;

procedure TArchFileHeader.LoadHeader(AParameters: TArchParamList);
var
  i: integer;
begin
  for i:= 0 to (Length(AParameters)-1) do
  begin
    case AParameters[i].ParamName of
     ARCH_HEADER_AUTHOR              : Author:= AParameters[i].Value;
     ARCH_HEADER_CREATIONDATE        : CreationDate:= StrToDate(AParameters[i].Value);
     ARCH_HEADER_CREATIONSYSTEM      : CreationSystem:= AParameters[i].Value;
     ARCH_HEADER_FILENAME            : FileName:= AParameters[i].Value;
     ARCH_HEADER_MODIFICATIONSYSTEM  : LastModificationSystem:= AParameters[i].Value;
     ARCH_HEADER_MODIFICATIONDATE    : ModificationDate:= StrToDate(AParameters[i].Value);
     ARCH_HEADER_OPENARCHVERSION     : OpenArchVersion:= StrToFloat(AParameters[i].Value);
    end;
  end;
end;

procedure TArchFileHeader.SetAuthor(AValue: string);
begin
  if FAuthor=AValue then Exit;
  FAuthor:=AValue;
end;

procedure TArchFileHeader.SetCreationDate(AValue: TDateTime);
begin
  if FCreationDate=AValue then Exit;
  FCreationDate:=AValue;
end;

procedure TArchFileHeader.SetCreationSystem(AValue: string);
begin
  if FCreationSystem=AValue then Exit;
  FCreationSystem:=AValue;
end;


{ ------------------------------------------------------------------------
                         TArchStreamReader
------------------------------------------------------------------------- }
function TArchStreamReader.ReadChar: Char;
begin
  if FStream.Read(Result, Sizeof(Char)) = 0 then Result := Char(cEOF);
end;

function TArchStreamReader.ReadLn: String;
var
  bufsize, charsRead: Integer;
  ch: Char;
begin
  bufsize := 256;
  charsRead := 0;
  SetLength(Result, bufsize);
  Repeat
    ch := ReadChar;
    Case ch Of
      #13: Begin {fim da linha }
            If ReadChar <> #10 Then FStream.Seek( -1, soFromCurrent );
           End; { Case #13 }
      #10, #26: Begin {alternativas pro fim da line }
                 ch := #13
                End
                Else Begin
                 Inc(charsRead);
                 If charsRead > bufsize Then
                  Begin
                   Inc(bufsize, 128);
                   SetLength(Result, bufsize );
                  End; { If }
                 Result[charsRead]:= ch;
                End; { Else }
    End; { Case }
  Until ch = #13;
  SetLength(Result, charsRead );
end;

constructor TArchStreamReader.Create(const AStream : TStream);
begin
  FStream:= AStream;
end;

class function TArchStreamReader.New(const AStream : TStream): TArchStreamReader;
begin
  Result:= TArchStreamReader.Create(AStream);
end;

function TArchStreamReader.CheckData(const ALine: String): string;
begin
  Result := Copy((Trim(ALine)),1,1);
end;

function TArchStreamReader.EndOfEntity(Param, Value: String): boolean;
begin
  Result := (Param=ARCH_STARTDATA) and(Value=ARCH_ENDENTITY);
end;

function TArchStreamReader.EndOSession(Param, Value: String): boolean;
begin
  Result := (Param=ARCH_STARTSESSION) and(Value=ARCH_ENDSESSION);
end;

procedure TArchStreamReader.ReadParameters(var Param, Value: String);
var
  Line: string;
  streamPosition:integer;
begin
  // Pula espaços em branco antes e caso não seja o final da classe ou final da secao,
  // atribui ao Param valores antes do =  e ao value valores apos o = ate antes do ;
  repeat
   Line := ReadLn;
  until (Length(Trim(Line)) > 0) or
        (Value = ARCH_END) or
        (FStream.Position >= FStream.Size);
   case Line of
     'LO': streamPosition:=0 ;

   end;
   case (CheckData(Line)) of
    ARCH_STARTSESSION :	begin
                            streamPosition:= Pos(ARCH_STARTSESSION,Line);
  			    Param:= Copy(Line,streamPosition,1);
  			    Value:= Copy(Line,streamPosition+1,(Length(Line)-1)-streamPosition);
                          end;
    ARCH_STARTDATA:	Begin
                            Param:= ARCH_STARTDATA;
                            Value:= Copy(Line,2,(Length(Line)-1)-1);
  	                  end
  			  else begin
  			    streamPosition:= Pos(':',Line);
  			    Param:= Copy(Line,1,streamPosition-1);
  			    Value:= Copy(Line,streamPosition+1,(Length(Line)-streamPosition)-1);
  			  end;
   end; // case
end;

{Thris function positions the stream right after the session sent}
function TArchStreamReader.GoToSession(ASession: string): boolean;
var
  vParam, vValue:string;
begin
  FStream.Position:= 0;
  try
   repeat
     ReadParameters(vParam,vValue);
     Result:=(vParam=ARCH_STARTSESSION) and (vValue=ASession);
   until Result;
  except
    Result:= false;
  end;
end;

function TArchStreamReader.Read3DPoint(const Value: String): IArch3DPoint;
var
  Values: TStringList;
begin
  Values:= TStringList.Create;
  Values.delimiter:= char(',');
  Values.delimitedText:= Copy(Value,Pos(Arch_PARAMSET[0],Value)+1,Pos(Arch_PARAMSET[1],Value)-2);
  Result := TArch3DPoint.New(StrtoFloat(Values[0]),
                         StrtoFloat(Values[1]),
                         StrtoFloat(Values[2]));
end;

function TArchStreamReader.ReadStringList(const AValue: String): TStringList;
begin
  Result:= TStringList.Create;
  Result.Delimiter:= Char(',');
  Result.DelimitedText:=Copy(AValue,Pos(Arch_PARAMSET[0],AValue)+1,Pos(Arch_PARAMSET[1],AValue)-2);
end;

{ ------------------------------------------------------------------------
                         TArchStreamWriter
------------------------------------------------------------------------- }
procedure TArchStreamWriter.WriteLn(Line: string);
begin
  // add a quebra de linha
  Line := Line + #13#10;
  // escreve a linha
  FStream.Write(Line[1], Length(Line));
end;

constructor TArchStreamWriter.Create(const AStream: TStream);
begin
  FStream:= AStream;
end;

class function TArchStreamWriter.New(const AStream: TStream): TArchStreamWriter;
begin
  Result := TArchStreamWriter.Create(AStream);
end;

procedure TArchStreamWriter.WriteParam(Param, Value: String);
begin
  // escreve o param e o value
  WriteLn(Param +':'+ Value+Arch_ENDPARAM);
end;

procedure TArchStreamWriter.Write3DPoint(A3DPoint: IArch3DPoint; ABaseParam: String);
begin
 Writeln(ABaseParam+':'+Arch_PARAMSET[0]+
           FloatToStr(A3DPoint.v(0))+','+
   	   FloatToStr(A3DPoint.v(1))+','+
   	   FloatToStr(A3DPoint.v(2))+
   	   Arch_PARAMSET[1]+Arch_ENDPARAM);
end;

procedure TArchStreamWriter.WriteSection(ASection: String);
begin
  // write the Section
  WriteLn(ARCH_STARTSESSION+ASection+ARCH_ENDPARAM);
end;

procedure TArchStreamWriter.WriteObject(AId: String);
begin
  // escreve o param e o value
  WriteLn(Arch_STARTDATA+AId+Arch_ENDPARAM);
end;

procedure TArchStreamWriter.WriteEndObject();
begin
  // write the end of the entities
  WriteLn(Arch_STARTDATA+Arch_ENDENTITY+Arch_ENDPARAM);
end;

{ -------------------------------------------------------------------
                          TArchIOStreamFile
  ------------------------------------------------------------------- }
constructor TArchIOStreamFile.Create(AStream: TStream);
begin
  FReader:= TArchStreamReader.New(AStream);
  FWriter:= TArchStreamWriter.New(AStream);
  FHeader:= TArchFileHeader.Create;
end;

destructor TArchIOStreamFile.Destroy;
begin
  FreeAndNil(FReader);
  FreeAndNil(FWriter);
  FreeAndNil(FHeader);
end;

class function TArchIOStreamFile.New(AStream: TStream):TArchIOStreamFile;
begin
  Result:= TArchIOStreamFile.Create(AStream);
end;

function TArchIOStreamFile.GetParamList(Sender:TObject):TArchParamList;
var
  PT : PTypeData; 
  PI : PTypeInfo; 
  I,J : Longint; 
  PP : PPropList; 
  prI : PPropInfo; 
  PropType: TTypeKind;
begin
  PI:= Sender.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
//  J:=GetPropList(PI,OrdinalTypes,PP);
  SetLength(Result,J);
  For I:=0 to J-1 do 
    begin 
      With PP^[i]^ do 
        begin 
//          Result[i].ParamName:= name:30; 
//          PropType:= TypeNames[typinfo.PropType(O,Kind)]; 
//          Result[i].Value:= ProcessValue(PropType,
        end; 
    end; 
  FreeMem(PP);
end;

procedure TArchIOStreamFile.SaveData(AParams: TArchParamList);
var
i: integer;
begin
  Writer.WriteObject(AParams[0].Value);
  for i:=1 to Length(AParams)-1 do
  begin
    Writer.WriteParam(AParams[i].ParamName, AParams[i].Value);
  end;
end;

function TArchIOStreamFile.LoadData(): TArchParamList;
var
  Param,Value: string;
  PosInStream,i: Int64;
  CheckEndEntity:Boolean;
begin
  i:=0;
  Repeat
     // Armazena a posicao na Stream antes de ler o parametro
     PosInStream:= Reader.ArchStream.Position;
     // Read the parameters
     Reader.ReadParameters(Param,Value);
     //Verifica se é o ultimo parametro da Entidade
     CheckEndEntity:= Reader.EndOfEntity(Param,Value);
     // Se nao for o fim da entidade, preenche o array de retorbo:
     if not CheckEndEntity then
       begin
         SetLength(Result, i+1);
         Result[i]:= TArchParameter.New(Param,Value);
         Result[i].Value:= Value;
         ShowMessage(Result[i].ParamName);
         ShowMessage(Result[i].Value);
         i:= i+1;
       end;
     //ShowMessage('Nao foi possivel interpretar o Parametro:' +#10#13+ Param);
     // Chegando ao fim da Entidade, reverte a Stream para a a posição Anterior
  until CheckEndEntity;
  // Retorna a Posição da Stream
  Reader.ArchStream.Seek(PosInStream, soBeginning);
end;

procedure TArchIOStreamFile.LoadHeader();
var
  Param,Value: string;
  vParamList: TArchParamList;
  i: Int64;
  CheckEndSession:Boolean;
begin
  i:=0;
  Reader.GoToSession(ARCH_SHEADER);
  Repeat
     // Read the parameters
     Reader.ReadParameters(Param,Value);
     //Verifica se é o ultimo parametro da Sessão Header
     CheckEndSession:= Reader.EndOSession(Param,Value);
     // Se nao for o fim da entidade, preenche o array de retorbo:
     if not CheckEndSession then
       begin
         SetLength(vParamList, i+1);
         vParamList[i]:= TArchParameter.New(Param,Value);
         i:= i+1;
       end;
     //ShowMessage('Nao foi possivel interpretar o Parametro:' +#10#13+ Param);
     // Chegando ao fim da Entidade, reverte a Stream para a a posição Anterior
  until CheckEndSession;
  // Update the Header of the file with de information in ParamList:
  FHeader.LoadHeader(vParamList);
  // Retorna a Posição da Stream
  Reader.ArchStream.Seek(0, soBeginning);
end;

function TArchIOStreamFile.LoadSection(ASection:String): TArchParamList;
var
  Param,Value: string;
  i: Int64;
  CheckEndSession:Boolean;
begin
  i:=0;
  Reader.GoToSession(ASection);
  Repeat
     // Read the parameters
     Reader.ReadParameters(Param,Value);
     //Verifica se é o ultimo parametro da Sessão Header
     CheckEndSession:= Reader.EndOSession(Param,Value);
     // Se nao for o fim da entidade, preenche o array de retorbo:
     if not CheckEndSession then
       begin
         SetLength(Result, i+1);
         Result[i]:= TArchParameter.New(Param,Value);
         i:= i+1;
       end;
     //ShowMessage('Nao foi possivel interpretar o Parametro:' +#10#13+ Param);
     // Chegando ao fim da Entidade, reverte a Stream para a a posição Anterior
  until CheckEndSession;
  // Update the Header of the file with de information in ParamList:
  // Retorna a Posição da Stream
  Reader.ArchStream.Seek(0, soBeginning);
end;

{ ------------------------------------------------------------------------
                         TArchXMLReader
------------------------------------------------------------------------- }
constructor TArchXMLReader.Create(const AXML: TXMLDocument);
begin
  FXML:= AXML;
end;

class function TArchXMLReader.New(const AXML: TXMLDocument): TArchXMLReader;
begin
  Result:= TArchXMLReader.Create(AXML);
end;

function TArchXMLReader.EndOfEntity(ANode:TDOMNode): boolean;
begin
  if ANode.NodeName= ARCH_ENDENTITY then Result:= True
    else Result:= false;
end;

procedure TArchXMLReader.ReadParameters(var Param,Value: String);
begin
  with FRootNode do
  begin
    Param:= NodeName;
    Value:= TextContent;
  end;
end;
   //Lê um ponto 3D
function TArchXMLReader.Read3DPoint(const Value:String): IArch3DPoint;
begin
  with FRootNode do
  begin
    if ChildNodes.Item[0].NodeName='X' then Result.SetValue(0,StrToFloat(ChildNodes.Item[0].TextContent));
    if ChildNodes.Item[1].NodeName='Y' then Result.SetValue(1, StrToFloat(ChildNodes.Item[1].TextContent));
    if ChildNodes.Item[2].NodeName='Z' then Result.SetValue(2, StrToFloat(ChildNodes.Item[2].TextContent));
  end;
end;  
   
{ ------------------------------------------------------------------------
                         TArchXMLWriter
------------------------------------------------------------------------- }
constructor TArchXMLWriter.Create(AXML: TXMLDocument);
begin
  FXML:= AXML;
end;

class function TArchXMLWriter.New(AXML: TXMLDocument): TArchXMLWriter;
begin
  Result:= TArchXMLWriter.Create(AXML);
end;

procedure TArchXMLWriter.WriteParam(Param,Value: String);
var
ParamNode,ValueNode:TDOMNode;
begin
  // Create the param node elemwnt and add to RootNod3.
  ParamNode:= FXML.CreateElement(Param);
  FRootNode.AppendChild(ParamNode);
  // Create the value text node element and add to parameter node
  ValueNode:= FXML.CreateTextNode(Value);
  ParamNode.AppendChild(ValueNode);
end;

//Escreve Um Ponto 3D
procedure TArchXMLWriter.Write3DPoint(A3DPoint: IArch3DPoint; ABaseParam: String);
var
XParamNode,XValueNode,
YParamNode,YValueNode,
ZParamNode,ZValueNode:TDOMNode;
begin
  // Write X Values
  XParamNode:= FXML.CreateElement('X');
  //FARootNode.AppendChild(XParamNode);
  XValueNode:= FXML.CreateTextNode(FloatToStr(A3dPoint.v(0)));
  XParamNode.AppendChild(XValueNode);
  // Write Y Values
  YParamNode:= FXML.CreateElement('Y');
  FRootNode.AppendChild(YParamNode);
  YValueNode:= FXML.CreateTextNode(FloatToStr(A3dPoint.v(1)));
  YParamNode.AppendChild(YValueNode);
  // Write Z Values
  ZParamNode:= FXML.CreateElement('Z');
  FRootNode.AppendChild(ZParamNode);
  ZValueNode:= FXML.CreateTextNode(FloatToStr(A3dPoint.v(2)));
  ZParamNode.AppendChild(ZValueNode);
end;

procedure TArchXMLWriter.WriteSection(ASection:String);
begin
  WriteParam(ARCH_STARTSESSION,ASection);
end;

Procedure TArchXMLWriter.WriteObject(AId:String);
begin
  WriteParam(ARCH_STARTDATA,AId);
end;

Procedure TArchXMLWriter.WriteEndObject();
begin
  WriteParam(ARCH_STARTDATA, ARCH_ENDENTITY);
end;

{ ------------------------------------------------------------------------
                         TArchXMLFile
------------------------------------------------------------------------- }

procedure TArchIOXMLFile.SaveData(AParams: TArchParamList);
var
NodeRoot:TDOMNode;
i: integer;
begin
  // Create the root node:
  with Writer do
  begin
    ArchXML.AppendChild(NodeRoot);
    // Configura no escritor qual será
    // o Nó raiz.
    RootNode:= NodeRoot;
    for i:= 1 to Length(AParams) -1 do
    begin
      WriteParam(AParams[i].ParamName,AParams[i].Value);
    end;
  end;
end;

destructor TArchIOXMLFile.Destroy;
begin
  FReader.Free;
  FWriter.Free;
  FXML.Free;
end;

function TArchIOXMLFile.LoadData: TArchParamList;
var
EndXNLEntity:boolean;
Node: TDOMNode;
Param,Value:String;
i: int64;
begin
  with Reader do
    begin
      Node:= ArchXML.DocumentElement.FirstChild;
      i:=0;
      // loop to read XMLEntity in file
      repeat
        EndXNLEntity:= EndOfEntity(Node);
        // Read parameters and values in nodes
        ReadParameters(Param,Value);
        if not EndXNLEntity then
          begin
            SetLength(Result, i+1);
            Result[i].ParamName:= Param;
            Result[i].Value:= Value;
            i:= i+1;
            Node:= Node.NextSibling;
          end;
      until EndXNLEntity;
    end;
end;

constructor TArchIOXMLFile.Create(AXMLPath: String);
begin
  ReadXMLFile(FXML,AXMLPath);
  FReader:= TArchXMLReader.New(FXML);
  FWriter:= TArchXMLWriter.New(FXML);
end;

class function TArchIOXMLFile.New(AXMLPath: String): TArchIOXMLFile;
begin
  Result:= TArchIOXMLFile.Create(AXMLPath);
end;

end. 
