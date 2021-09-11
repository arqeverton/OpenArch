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
 2021 08 21 - ET - Unit implementation Start's.
}

unit OpenArchGraphicResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //OpenGL:
  GL, GLext,
  //OpenArch:
  OpenArchSysParameters, OpenArchVectorGeometry, OpenArchColor;

type

  TArchLineStyle = (lsSolid, lsDashed, lsSimbolic);
  TArchSDLQualifiers = (sdlAttribute, sdlUniform, sdlVarying, sdlConst, sdlAbsent);
  TArchSDLType = (sdlInt, sdlFloat, sdlBool, sdlVec2, sdlVec3, sdlVec4, sdlIvec2, sdlIvev3,
                  sdlIvec4, sdlBvec2, sdlBvec3, sdlBvec4, sdlMat2, sdlMmat3, sdlMmat4,
                  sdlSampler1, sdalSampler2, sdlSampler3, sdlSamplerCube, sdlSampler1DShadow,
                  sdlSampler2DShadow, sdlStruct, sdlArray, sdlVoid);
  TArchSDLParameter = Class(TArchParameter)
    private
      FSDLQualifiers: TArchSDLQualifiers ;
      FSDLType: TArchSDLType;
    public
      property SDLQualifiers:TArchSDLQualifiers read FSDLQualifiers write FSDLQualifiers;
      property SDLType:TArchSDLType read FSDLType write FSDLType;
  end;

  TArchSDLParameters = Array of TArchSDLParameter;

  {---------------------------------------------------------------------------
                            TArchGraphicSDLBased
  ----------------------------------------------------------------------------}

   { TArchGraphicSDLBased }

   TArchGraphicSDLBased = class
     private
       FVertexShaderCode,
       FFragmentShaderCode: PGLchar;
       FVertexShaderProgram,
       FFragmentShaderProgram: TStringList;
       FVertexShaderID,
       FFragmentShaderID,
       FProgramID: GLuint;
       FInfoLogLength,
       FCompilationResult: GLint;
       FColors: Array of TArchColor;
       FSDLParameters: TArchSDLParameters;
       procedure LoadOpenGL();
       procedure CreateAndLoadShaders();
       procedure CompileVertexShader();
       procedure CompileFragmentShader();
       procedure LinkProgram();
     protected

     public
       procedure AddParam(AValue: TArchSDLParameter);
       constructor Create(); virtual;
       destructor Destroy;
       procedure Apply(); virtual;
       procedure UnApply(); virtual;
       property VertexShaderProgram:TStringList read FVertexShaderProgram write FVertexShaderProgram;
       property FragmentShaderProgram:TStringList read FFragmentShaderProgram write FFragmentShaderProgram;
   end;

  {---------------------------------------------------------------------------
                            TArchLineType
  ----------------------------------------------------------------------------}

  { TArchLineType }

  TArchLineType = class
    private
      FLineTypeID: integer;
      FLineTypeName:String;
      FLineTypeDash,
      FLineTypeGap: Array of Single;
      FLineStyle: TArchLineStyle;
      function GetDashes: integer;
      function GetGaps: integer;
      function GetLineTypeDash(Index: integer): Single;
      function GetLineTypeGap(Index: integer): Single;
      procedure SetLineStyle(AValue: TArchLineStyle);
      procedure SetLineTypeDash(Index: integer; AValue: Single);
      procedure SetLineTypeGap(Index: integer; AValue: Single);
      procedure SetLineTypeID(AValue: integer);
      procedure SetLineTypeName(AValue: String);
      procedure AddDash(ADash:Single);
      procedure AddGap(AGap:Single);
    public
      function SumOfDashes():single;
      function SumOfGaps():single;
      procedure LoadDashes(AStringList:TStringList);
      procedure LoadGaps(AStringList:TStringList);
      procedure AddDashed(ADash,AGap:single);
      procedure DelDashed(AIndex:integer);
      property LineTypeID:integer read FlineTypeID write SetLineTypeID;
      property LineTypeName:String read FLineTypeName write SetLineTypeName;
      property LineTypeDash[Index:integer]:Single read GetLineTypeDash write SetLineTypeDash;
      property LineTypeGap[Index:integer]:Single read GetLineTypeGap write SetLineTypeGap;
      property LineStyle:TArchLineStyle read FLineStyle write SetLineStyle;
      property DashesCount:integer read GetDashes;
      property GapsCount:integer read GetGaps;
    end;

  TArchLineTypeArray = Array of TArchLineType;
  {---------------------------------------------------------------------------
                            TArchLineTypes
  ----------------------------------------------------------------------------}

  { TArchLineTypes }

  TArchLineTypes = class
    private
      FLineTypes: TArchLineTypeArray;
      function GetCount: integer;
      function GetLineType(Index: integer): TArchLineType;
      function GetLineTypeByName(AName: String): TArchLineType;
      procedure SetLineTypeByName(AName: String; AValue: TArchLineType);
    public
      constructor Create;
      procedure LoadData(AParameters: TArchParamList);
      procedure AddLineType(ALineType: TArchLineType);
      procedure AddLineType(AName: string; AStyle:TArchLineStyle); overload;
      function CheckLineTypeNameExist(AName:string):Boolean;
      procedure AddLineType(AName:string); overload;
      procedure DelLineType(AName:string);
      destructor Destroy; override;
      property Count:integer read GetCount;
      property LineType[Index:integer]:TArchLineType read GetLineType;
      property LineTypeByName[AName:String]:TArchLineType read GetLineTypeByName write SetLineTypeByName;

  end;

  TArchFillStyle = (fsSolid, fsHatch, fsGradient);
  TArchGradientType = (gtRadial, gtLinear, gtCubic, gtNone);

  {---------------------------------------------------------------------------
                            TArchHatch
  ----------------------------------------------------------------------------}

  { TArchHatch }

  TArchHatch = class(TArchGraphicSDLBased)
    private

    public
      constructor Create(); override;
      destructor Destroy;
      procedure Apply(); override;
  end;


  {---------------------------------------------------------------------------
                            TArchFillType
  ----------------------------------------------------------------------------}

  { TArchFillType }

  TArchFillType = Class
    private
      FFillTypeID:integer;
      FFillStyle: TArchFillStyle;
      FFillTypeName: String;
      FFillTypePattern: word;
      FGradientType: TArchGradientType;
      procedure SetFillTypeID(AValue: integer);
      procedure SetFillTypeName(AValue: string);
    public
      property FillTypeID:integer read FFillTypeID write SetFillTypeID;
      property FillTypeName:string read FFillTypeName write SetFillTypeName;
      // This Procedure enable a shader if Fill Style is a Hatch or Gradient:
      procedure EnableShader();
      procedure DisableShader();
  end;



  TArchFillTypeArray = array of TArchFillType;

  { TArchFillTypes }

  TArchFillTypes = class
    private
      FFillTypes: TArchFillTypeArray;
    public
      procedure LoadData(AParameters: TArchParamList);
      destructor Destroy; override;
  end;

  {---------------------------------------------------------------------------
                            TArchGradientType
  ----------------------------------------------------------------------------}

  TArchGraphicResources= class
    private
      FLineTypes: TArchLineTypes;
      FFillTypes: TArchFillTypes;
    public
      constructor Create();
      procedure LoadData(AParameters: TArchParamList);
      destructor Destroy; override;

      property LineTypes:TArchLineTypes read FLineTypes;
      property FillTypes:TArchFillTypes read FFillTypes;
  end;


implementation
uses
  //FPC:
  Dialogs;

{---------------------------------------------------------------------------
                            TArchGraphicSDLBased
----------------------------------------------------------------------------}

procedure TArchGraphicSDLBased.LoadOpenGL();
begin
  //init OpenGL and load extensions
  if Load_GL_VERSION_4_0 = false then
    if Load_GL_VERSION_3_3 = false then
      if Load_GL_VERSION_3_2 = false then
        if Load_GL_VERSION_3_0 = false then
        begin
          ShowMessage('Atenção: OPENARCH precisa de OpenGL 3.0 ou superior');
          HALT;
        end;
end;

procedure TArchGraphicSDLBased.CreateAndLoadShaders();
begin
  //creating shaders
  FVertexShaderID := glCreateShader( GL_VERTEX_SHADER );
  FFragmentShaderID := glCreateShader( GL_FRAGMENT_SHADER );
  //load shader code and get PChars
  FVertexShaderCode := FVertexShaderProgram.GetText;
  if FVertexShaderCode = nil then HALT;
  FFragmentShaderCode := FFragmentShaderProgram.GetText;
  if FFragmentShaderCode = nil then HALT;
end;

procedure TArchGraphicSDLBased.CompileVertexShader();
var
  i:word;
  ErrorMessageArray: array of GLChar;
begin
  //compiling and error checking vertex shader
  ShowMessage('Compilando e verificando o Vertex Shader ... ' );
  glShaderSource( FVertexShaderID, 1, @FVertexShaderCode, nil );
  glCompileShader( FVertexShaderID );
  glGetShaderiv( FVertexShaderID, GL_COMPILE_STATUS, @FCompilationResult );
  glGetShaderiv( FVertexShaderID, GL_INFO_LOG_LENGTH, @FInfoLogLength );
  if FCompilationResult = GL_FALSE then
  begin
    ShowMessage( 'Falha ao compilar o Vertex Shader ' );
    SetLength( ErrorMessageArray, FInfoLogLength+1 );
    glGetShaderInfoLog( FVertexShaderID, FInfoLogLength, nil, @ErrorMessageArray[0] );
    for i := 0 to FInfoLogLength do write( String( ErrorMessageArray[i] ) );
    ;
  end  else ShowMessage( 'success' );
end;

procedure TArchGraphicSDLBased.CompileFragmentShader();
var
  i:word;
  ErrorMessageArray: array of GLChar;
begin
  //compiling and error checking fragment shader
  ShowMessage('Compilando e verificando erros do Fragment Shader ... ' );
  glShaderSource( FFragmentShaderID, 1, @FFragmentShaderCode, nil );
  glCompileShader( FFragmentShaderID );
  glGetShaderiv( FFragmentShaderID, GL_COMPILE_STATUS, @FCompilationResult );
  glGetShaderiv( FFragmentShaderID, GL_INFO_LOG_LENGTH, @FInfoLogLength );
  if FCompilationResult = GL_FALSE then
  begin
    ShowMessage( 'Falha ao compilar o  fragment shader' );
    SetLength( ErrorMessageArray, FInfoLogLength+1 );
    glGetShaderInfoLog( FVertexShaderID, FInfoLogLength, nil, @ErrorMessageArray[0] );
    for i := 0 to FInfoLogLength do write( String( ErrorMessageArray[i] ) );
    ;
  end  else ShowMessage( 'success' );
end;

procedure TArchGraphicSDLBased.LinkProgram();
var
  i:word;
  ErrorMessageArray: array of GLChar;
begin
  //creating and linking program
  //ShowMessage('Criando e Linkando o Programa... ' );
  FProgramID := glCreateProgram();
  glAttachShader( FProgramID, FVertexShaderID );
  glAttachShader( FProgramID, FFragmentShaderID );
  glLinkProgram( FProgramID );
  glGetShaderiv( FProgramID, GL_LINK_STATUS, @FCompilationResult );
  glGetShaderiv( FProgramID, GL_INFO_LOG_LENGTH, @FInfoLogLength );
  if FCompilationResult = GL_FALSE then
  begin
    //ShowMessage( 'Falha ' );
    SetLength( ErrorMessageArray, FInfoLogLength+1 );
    glGetShaderInfoLog( FVertexShaderID, FInfoLogLength, nil, @ErrorMessageArray[0] );
    for i := 0 to FInfoLogLength do write( String( ErrorMessageArray[i] ) );
    ;
  end; //else ShowMessage( 'success' );
end;

procedure TArchGraphicSDLBased.AddParam(AValue: TArchSDLParameter);
var
  i: integer;
begin
  SetLength(FSDLParameters,Length(FSDLParameters)+1);
  FSDLParameters[Length(FSDLParameters)-1]:= AValue;
end;

constructor TArchGraphicSDLBased.Create();
begin
  FVertexShaderProgram:= TStringList.Create;
  FFragmentShaderProgram:= TStringList.Create;
  FCompilationResult:= GL_FALSE;
end;

destructor TArchGraphicSDLBased.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FVertexShaderCode);
  FreeAndNil(FFragmentShaderCode);
end;

procedure TArchGraphicSDLBased.Apply();
var
  InfoLogLength: GLint;
  i:word;
  ErrorMessageArray: array of GLChar;
begin

  LoadOpenGL();
  CreateAndLoadShaders();
  CompileVertexShader();
  CompileFragmentShader();
  LinkProgram();

  glUseProgram( FProgramID );
  glUniform3f(glGetUniformLocation(FProgramID, 'BrickColor'), 1.0, 1.0, 1.0);
  glUniform3f(glGetUniformLocation(FProgramID, 'MortarColor'), 0.0, 0.0, 0.0);
  glUniform2f(glGetUniformLocation(FProgramID, 'BrickSize'), 0.5, 0.5);
  glUniform2f(glGetUniformLocation(FProgramID, 'BrickPct'), 0.2, 0.2);
  //glUniform3f(glGetUniformLocation(FProgramID, 'LightPosition'), 0,
  //                                                               0,
  //                                                               2);
end;

procedure TArchGraphicSDLBased.UnApply();
begin
  //clean up
  glDetachShader( FProgramID, FVertexShaderID );
  glDetachShader( FProgramID, FFragmentShaderID );

  glDeleteShader( FVertexShaderID );
  glDeleteShader( FFragmentShaderID );
  glDeleteProgram( FProgramID );

  StrDispose( FVertexShaderCode );
  StrDispose( FFragmentShaderCode );
end;

{---------------------------------------------------------------------------
                            TArchHatch
----------------------------------------------------------------------------}

constructor TArchHatch.Create();
begin
  inherited Create;
end;

destructor TArchHatch.Destroy;
begin

end;

procedure TArchHatch.Apply();
begin
  inherited;
end;

{---------------------------------------------------------------------------
                             TArchFillTypes
----------------------------------------------------------------------------}

procedure TArchFillTypes.LoadData(AParameters: TArchParamList);
begin

end;

destructor TArchFillTypes.Destroy;
begin
  inherited Destroy;
end;

{---------------------------------------------------------------------------
                             TArchLineTypes
----------------------------------------------------------------------------}

function TArchLineTypes.GetCount: integer;
begin
  Result:= Length(FLineTypes);
end;

function TArchLineTypes.GetLineType(Index: integer): TArchLineType;
begin
  Result:= FLineTypes[Index];
end;

function TArchLineTypes.GetLineTypeByName(AName: String): TArchLineType;
var
  i:integer;
begin
  try
    for i:= 0 to Count-1 do
    begin
       if FLineTypes[i].LineTypeName= AName then
         Result:= FLineTypes[i];
    end;
  except
    Result:= nil;
  end;
end;

procedure TArchLineTypes.SetLineTypeByName(AName: String; AValue: TArchLineType
  );
begin

end;

constructor TArchLineTypes.Create;
begin

end;

procedure TArchLineTypes.LoadData(AParameters: TArchParamList);
var
  i:Int64;
  vLineType:TArchLineType;
  vCreateLineType:Boolean;
begin
  DecimalSeparator:='.';
  vCreateLineType:=False;
  for i:=0 to Length(AParameters)-1 do
  begin
    case AParameters[i].ParamName of
      ARCH_STARTDATA:
        begin
         if not (AParameters[i].Value=ARCH_ENDENTITY) then
         begin
           vLineType:=TArchLineType.Create();
           vCreateLineType:=True;
         end else
         begin
           if vCreateLineType then
           begin
            AddLineType(vLineType);
            vCreateLineType:=False;
           end;
         end;
        end;
      ARCH_LINE_TYPE_ID   : vLineType.LineTypeID:= AParameters[i].Value.ToInteger;
      ARCH_LINE_TYPE_NAME : vLineType.LineTypeName:= AParameters[i].Value;
      ARCH_LINE_STYLE     :
        begin
          case AParameters[i].Value.ToInteger of
            0 : vLineType.LineStyle:= lsSolid;
            1 : vLineType.LineStyle:= lsDashed;
            2 : vLineType.LineStyle:= lsSimbolic;
          end;
        end;
      ARCH_LINE_TYPE_DASH : vLineType.LoadDashes(AParameters[i].GetStringList());
      ARCH_LINE_TYPE_GAP : vLineType.LoadGaps(AParameters[i].GetStringList());
    end;
  end;
  vLineType:= Nil;
  FreeAndNil(vLineType);
  DecimalSeparator:=',';
end;

procedure TArchLineTypes.AddLineType(ALineType: TArchLineType);
begin
  SetLength(FLineTypes,Length(FLineTypes)+1);
  FLineTypes[Length(FLineTypes)-1]:=ALineType;
end;

procedure TArchLineTypes.AddLineType(AName: string;
  AStyle: TArchLineStyle);
var
  vLineType:TArchLineType;
  i:integer;
begin
  if not CheckLineTypeNameExist(AName) then
  begin
    vLineType:=TArchLineType.Create();
    vLineType.LineTypeName:=AName;
    vLineType.LineStyle:= AStyle;
    AddLineType(vLineType);
  end;
end;

function TArchLineTypes.CheckLineTypeNameExist(AName: string): Boolean;
var
  i:integer;
  vCheck:Boolean;
begin
  vCheck:=False;
  for i:= 0 to Count -1 do
  begin
   if LineType[i].LineTypeName= AName then vCheck:=True;
  end;
  Result:= vCheck;
end;

procedure TArchLineTypes.AddLineType(AName: string);
var
  vLineType:TArchLineType;
  i:integer;
begin
  if not CheckLineTypeNameExist(AName) then
  begin
    vLineType:=TArchLineType.Create();
    vLineType.LineTypeName:=AName;
    vLineType.LineStyle:= lsSolid;
    AddLineType(vLineType);
  end;
end;

procedure TArchLineTypes.DelLineType(AName: string);
var
  i:integer;
  lineTypeAux:TArchLineTypeArray;
begin
  SetLength(lineTypeAux,0);
  for i:=0 to Count-1 do
  begin
    if not (FLineTypes[i].LineTypeName=AName)then
    begin
      SetLength(lineTypeAux,Length(lineTypeAux)+1);
      lineTypeAux[Length(lineTypeAux)-1]:= FLineTypes[i];
    end else
    begin
      FreeAndNil(FLineTypes[i]);
    end;
  end;
  SetLength(FLineTypes,Length(lineTypeAux));
  for i:=0 to Length(lineTypeAux)-1 do
  begin
    FLineTypes[i]:= lineTypeAux[i];
  end;
end;

destructor TArchLineTypes.Destroy;
begin
  inherited Destroy;
end;

{---------------------------------------------------------------------------
                         TArchGraphicResources
----------------------------------------------------------------------------}

constructor TArchGraphicResources.Create;
begin
  FLineTypes:= TArchLineTypes.Create;
  FFillTypes:= TArchFillTypes.Create;
end;

procedure TArchGraphicResources.LoadData(AParameters: TArchParamList);
var
  i,vCount:integer;
  vLineType, vFillType:Boolean;
  vLineTParams, vFillTParams: TArchParamList;
begin
  vLineType:=false;
  vFillType:=false;
  vCount:=0;
  for i:= 0 to Length(AParameters)-1 do
  begin
    case AParameters[i].ParamName of
      ARCH_STARTDATA :
        begin
          case AParameters[i].Value of
            ARCH_LINE_TYPES :
              begin
                vLineType:=true;
                vFillType:=false;
              end;
            ARCH_FILL_TYPES :
              begin
                vLineType:=False;
                vFillType:=True;
              end;
          end;
        end;
    end;
    if vLineType then
    begin
      SetLength(vLineTParams,(Length(vLineTParams)+1));
      vLineTParams[(Length(vLineTParams))-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
    end;
    if vFillType then
    begin
      SetLength(vFillTParams,(Length(vFillTParams)+1));
      vFillTParams[(Length(vFillTParams))-1]:= TArchParameter.Create(AParameters[i].ParamName,AParameters[i].Value);
    end;
  end; // Finish For
  LineTypes.LoadData(vLineTParams);
  //FillTypes.LoadData(vFillTParams);
end;

destructor TArchGraphicResources.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLineTypes);
  FreeAndNil(FFillTypes);
end;

{---------------------------------------------------------------------------
                           TArchFillType
----------------------------------------------------------------------------}

procedure TArchFillType.SetFillTypeID(AValue: integer);
begin
  if FFillTypeID=AValue then Exit;
  FFillTypeID:=AValue;
end;

procedure TArchFillType.SetFillTypeName(AValue: string);
begin
  if FFillTypeName=AValue then Exit;
  FFillTypeName:=AValue;
end;

procedure TArchFillType.EnableShader();
begin

end;

procedure TArchFillType.DisableShader();
begin

end;


{---------------------------------------------------------------------------
                           TArchLineType
----------------------------------------------------------------------------}

procedure TArchLineType.SetLineTypeName(AValue: String);
begin
  if FLineTypeName=AValue then Exit;
  FLineTypeName:=AValue;
end;

procedure TArchLineType.AddDash(ADash: Single);
begin
  SetLength(FLineTypeDash,Length(FLineTypeDash)+1);
  FLineTypeDash[Length(FLineTypeDash)-1]:= ADash;
end;

procedure TArchLineType.AddGap(AGap: Single);
begin
  SetLength(FLineTypeGap,Length(FLineTypeGap)+1);
  FLineTypeGap[Length(FLineTypeGap)-1]:= AGap;
end;

function TArchLineType.SumOfDashes(): single;
var i:integer;
begin
  Result:=0;
  if Length(FLineTypeDash)>0 then
  begin
   for i:=0 to Length(FLineTypeDash)-1 do
   begin
     Result:= Result+FLineTypeDash[i];
   end;
  end else
  begin
   Result:=0;
  end;
end;

function TArchLineType.SumOfGaps(): single;
var i:integer;
begin
  Result:=0;
  if Length(FLineTypeGap)>0 then
  begin
   for i:=0 to Length(FLineTypeGap)-1 do
   begin
     Result:= Result+FLineTypeGap[i];
   end;
  end else
  begin
   Result:=0;
  end;
end;

procedure TArchLineType.SetLineTypeID(AValue: integer);
begin
  if FlineTypeID=AValue then Exit;
  FlineTypeID:=AValue;
end;

function TArchLineType.GetLineTypeDash(Index: integer): Single;
begin
  Result:= FLineTypeDash[Index];
end;

function TArchLineType.GetDashes: integer;
begin
  Result:= Length(FLineTypeDash);
end;

function TArchLineType.GetGaps: integer;
begin
  Result:= Length(FLineTypeGap);
end;

function TArchLineType.GetLineTypeGap(Index: integer): Single;
begin
  Result:= FLineTypeGap[Index];
end;

procedure TArchLineType.SetLineStyle(AValue: TArchLineStyle);
begin
  if FLineStyle=AValue then Exit;
  if (AValue= lsDashed) and ((Length(FLineTypeDash)=0) and (Length(FLineTypeGap)=0)) then
  begin
   AddDashed(0,0.05);
  end;
  FLineStyle:=AValue;
end;

procedure TArchLineType.SetLineTypeDash(Index: integer; AValue: Single);
begin
  FLineTypeDash[Index]:= AValue;
end;

procedure TArchLineType.SetLineTypeGap(Index: integer; AValue: Single);
begin
  FLineTypeGap[Index]:= AValue;
end;

procedure TArchLineType.LoadDashes(AStringList: TStringList);
var
  i:integer;
begin
  SetLength(FLineTypeDash,AStringList.Count);
  for i:=0 to Length(FLineTypeDash)-1 do
  begin
    FLineTypeDash[i]:= AStringList[i].ToSingle;
  end;
end;

procedure TArchLineType.LoadGaps(AStringList: TStringList);
var
  i:integer;
begin
  SetLength(FLineTypeGap,AStringList.Count);
  for i:=0 to Length(FLineTypeGap)-1 do
  begin
    FLineTypeGap[i]:= AStringList[i].ToSingle;
  end;
end;

procedure TArchLineType.AddDashed(ADash, AGap: single);
begin
  AddDash(ADash); AddGap(AGap);
end;

procedure TArchLineType.DelDashed(AIndex: integer);
var
  i:integer;
  arrayAux,arrayGapAux:array of Single;
begin
  if Length(FLineTypeDash)=1 then
  begin
   ShowMessage('Para tipos tracejados ao menos'+LineEnding+
               'um traço e vazio são necessários!');

  end else
  begin
    SetLength(arrayAux,0);
    SetLength(arrayGapAux,0);
    for i:=0 to Length(FLineTypeDash)-1 do
    begin
      if not (i=AIndex)then
      begin
        SetLength(arrayAux,Length(arrayAux)+1);
        SetLength(arrayGapAux,Length(arrayGapAux)+1);
        arrayAux[Length(arrayAux)-1]:= FLineTypeDash[i];
        arrayGapAux[Length(arrayGapAux)-1]:= FLineTypeGap[i];
      end;
    end;
    SetLength(FLineTypeDash,Length(arrayAux));
    SetLength(FLineTypeGap,Length(arrayGapAux));
    for i:=0 to Length(arrayAux)-1 do
    begin
      FLineTypeDash[i]:= arrayAux[i];
      FLineTypeGap[i]:= arrayGapAux[i];
    end;
  end;
end;

end.

