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
 2021 04 30 - ET - Unit implementation Start's.
}
unit OpenArchLayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  //OpenArch:
  OpenArchSysParameters;

type

{---------------------------------------------------------------------------
                          TArchLayer
----------------------------------------------------------------------------}

  { TArchLayer }

  TArchLayer = class
    private
      FLayerName: String;
      FLocked,
      FVisible,
      FPrintable: Boolean;
      procedure SetLayerName(AValue: string);
      procedure SetLocked(AValue: Boolean);
      procedure SetPrintable(AValue: Boolean);
      procedure SetVisible(AValue: Boolean);
    public
      constructor Create(AName:string; Alock, AVisble, APrint:Boolean);overload;
      property LayerName:string read FLayerName write SetLayerName;
      property Locked:Boolean read FLocked write SetLocked;
      property Visible:Boolean read FVisible write SetVisible;
      property Printable:Boolean read FPrintable write SetPrintable;
  end;

  TArchLayerArray = Array of TArchLayer;
{---------------------------------------------------------------------------
                          TArchLayers
----------------------------------------------------------------------------}

  { TArchLayers }

  TArchLayers = class
    private
      FLayers:TArchLayerArray;
      function GetLayer(Index: Int64): TArchLayer;
      function GetLayerByName(ALayerName: string): TArchLayer;
      function GetLayersCount: integer;
      procedure SetLayer(Index: Int64; AValue: TArchLayer);
      procedure SetLayers(AValue: TArchLayerArray);
    public
      procedure LoadData(AParameters: TArchParamList);
      procedure AddLayer(ALayer:TArchLayer);
      procedure DelLayer(ALayerName:string);
      procedure EditLayer(ALayerOldName,ALayerNewName,ALocked,AVisibles,APrintable:string);
      procedure Clone(ALayers: TArchLayers);
      property Layers:TArchLayerArray read FLayers write SetLayers;
      property Layer[Index:Int64]:TArchLayer read GetLayer write SetLayer;
      property LayerByName[ALayerName:string]:TArchLayer read GetLayerByName;
      property LayersCount:integer read GetLayersCount;
  end;

{---------------------------------------------------------------------------
                          TArchLayersGroup
----------------------------------------------------------------------------}

  { TArchLayersGroup }

  TArchLayersGroup = class
    private
      FLayerGroupName: string;
      FLockeds,
      FVisibles,
      FPrintables: TStringList;
      procedure SetLayerGroupName(AValue: String);
      procedure SetLockeds(AValue: TStringList);
      procedure SetPrintables(AValue: TStringList);
      procedure SetVisibles(AValue: TStringList);
    public
      constructor Create();
      destructor Destroy();
      procedure DelAllLockeds();
      procedure DelAllVisibles();
      procedure DelAllPrintables();
      procedure Clone(ALayerGroup:TArchLayersGroup);
      property LayerGroupName:String read FLayerGroupName write SetLayerGroupName;
      property Lockeds: TStringList read FLockeds write SetLockeds;
      property Visibles:TStringList read FVisibles write SetVisibles;
      property Printables:TStringList read FPrintables write SetPrintables;
  end;

  TArchLayersGroupArray = Array of TArchLayersGroup;
{---------------------------------------------------------------------------
                          TArchLayersSet
----------------------------------------------------------------------------}

  { TArchLayersSet }

  TArchLayersSet = class
    private
      FLayersSet: TArchLayersGroupArray;
      function GetLayerGroup(Index: Int64): TArchLayersGroup;
      function GetLayerGroupByName(ALayerGroupName: string): TArchLayersGroup;
      function GetLayerSetCount: Int64;
      procedure SetLayerGroupName(Index: Int64; AValue: TArchLayersGroup);
    public
      constructor Create;
      procedure LoadData(AParameters:TArchParamList);
      procedure Clone(ALayerSet: TArchLayersSet);
      procedure AddLayerGroup(AName:String;ALockeds,AVisibles,APrintables:TStringList);
      procedure DelLayerGroup(AName:string);
      procedure EditLayerGroupName(AName,ANewName:String);
      property LayerGroup[Index:Int64]: TArchLayersGroup read GetLayerGroup write SetLayerGroupName;
      property LayerGroupByName[ALayerGroupName:string]:TArchLayersGroup read GetLayerGroupByName;
      property LayerSetCount:Int64 read GetLayerSetCount;
    end;

implementation
uses Dialogs;

{---------------------------------------------------------------------------
                          TArchLayersSet
----------------------------------------------------------------------------}

function TArchLayersSet.GetLayerGroup(Index: Int64): TArchLayersGroup;
begin
  try
    Result:=FLayersSet[Index];
  except
    Result:= nil;
  end;
end;

function TArchLayersSet.GetLayerGroupByName(ALayerGroupName: string): TArchLayersGroup;
var
  i:int64;
begin
  try
    for i:= 0 to Length(FLayersSet)-1 do
    begin
       if FLayersSet[i].LayerGroupName= ALayerGroupName then
         Result:= FLayersSet[i];
    end;
  except
    Result:= nil;
  end;
end;

function TArchLayersSet.GetLayerSetCount: Int64;
begin
  Result:= Length(FLayersSet);
end;

procedure TArchLayersSet.SetLayerGroupName(Index: Int64;
  AValue: TArchLayersGroup);
begin
  try
    FLayersSet[Index]:= AValue;
  except
    ShowMessage('Camada Inexistente"');
    Abort;
  end;
end;

constructor TArchLayersSet.Create;
begin
  SetLength(FLayersSet,1);
  FLayersSet[0]:= TArchLayersGroup.Create;
  FLayersSet[0].LayerGroupName:='_Conjunto de Camadas OpenArch';
  with FLayersSet[0] do
  begin
    FLockeds:= TStringList.Create;
    FVisibles:= TStringList.Create;
    FPrintables:= TStringList.Create;
    FLockeds.Add(ARCH_ALL);
    FVisibles.Add(ARCH_ALL);
    FPrintables.Add(ARCH_ALL);
  end;
end;

procedure TArchLayersSet.LoadData(AParameters: TArchParamList);
var
  i:Int64;
begin
  for i:=0 to Length(AParameters)-1 do
  begin
    case AParameters[i].ParamName of
      ARCH_STARTDATA:
        begin
         if not (AParameters[i].Value=ARCH_ENDENTITY) then
         begin
           SetLength(FLayersSet,Length(FLayersSet)+1);
           FLayersSet[Length(FLayersSet)-1]:= TArchLayersGroup.Create;
         end;
        end;
      ARCH_LAYER_GROUP_NAME :
        begin
          FLayersSet[Length(FLayersSet)-1].LayerGroupName:= AParameters[i].Value;
        end;
      ARCH_LAYER_LOCKEDS :
        begin
          with FLayersSet[Length(FLayersSet)-1] do
          begin
            FLockeds:=AParameters[i].GetStringList();
          end;
        end;
      ARCH_LAYER_VISIBLES :
        begin
         with FLayersSet[Length(FLayersSet)-1] do
         begin
           FVisibles:=AParameters[i].GetStringList();
         end;
        end;
      ARCH_LAYER_PRINTABLES :
        begin
         with FLayersSet[Length(FLayersSet)-1] do
         begin
           FPrintables:=AParameters[i].GetStringList();
         end;
        end;
    end;
  end;
end;

procedure TArchLayersSet.Clone(ALayerSet: TArchLayersSet);
var
  i:integer;
begin
  SetLength(Self.FLayersSet,0);
  if Assigned(ALayerSet) then
  begin
    for i:=0 to ALayerSet.LayerSetCount-1 do
    begin
      SetLength(Self.FLayersSet,i+1);
      Self.FLayersSet[i]:= TArchLayersGroup.Create;
      Self.FLayersSet[i].Clone(ALayerSet.LayerGroup[i]);
    end;
  end;
end;

procedure TArchLayersSet.AddLayerGroup(AName: String; ALockeds, AVisibles,
  APrintables: TStringList);
var
  i:Integer;
begin
  SetLength(FLayersSet, Length(FLayersSet)+1);
  FLayersSet[Length(FLayersSet)-1]:=TArchLayersGroup.Create;
  FLayersSet[Length(FLayersSet)-1].LayerGroupName:=AName;
  for i:=0 to ALockeds.Count-1 do
  begin
   FLayersSet[Length(FLayersSet)-1].FLockeds.Add(ALockeds.Strings[i]);
  end;
  for i:=0 to AVisibles.Count-1 do
  begin
   FLayersSet[Length(FLayersSet)-1].FVisibles.Add(AVisibles.Strings[i]);
  end;
  for i:=0 to APrintables.Count-1 do
  begin
   FLayersSet[Length(FLayersSet)-1].FPrintables.Add(APrintables.Strings[i]);
  end;
end;

procedure TArchLayersSet.DelLayerGroup(AName: string);
var
  lgaux:TArchLayersGroupArray;
  i:Integer;
begin
  if AName='_Conjunto de Camadas OpenArch' then
  begin
    ShowMessage('Impossível Apagar essa Camada!');
  end else
  begin
    SetLength(lgaux,0);
    for i:= 0 to LayerSetCount-1do
    begin
     if not (FLayersSet[i].LayerGroupName= AName) then
     begin
       SetLength(lgaux,Length(lgaux)+1);
       lgaux[Length(lgaux)-1]:= FLayersSet[i];
     end;
    end;
    SetLength(FLayersSet,Length(lgaux));
    for i:=0 to Length(lgaux)-1 do
    begin
      FLayersSet[i]:= lgaux[i];
    end;
  end;
end;

procedure TArchLayersSet.EditLayerGroupName(AName, ANewName: String);
var
  i:Integer;
begin
  if (AName='_Conjunto de Camadas OpenArch') then
  begin
    ShowMessage('Impossível Editar essa Camada!');
  end else
  begin
    for i:= 0 to LayerSetCount-1do
    begin
     if (FLayersSet[i].LayerGroupName= AName) then
     begin
       LayerGroup[i].LayerGroupName:=ANewName;
       Break;
     end;
    end;
  end;
end;

{---------------------------------------------------------------------------
                          TArchLayers
----------------------------------------------------------------------------}
procedure TArchLayers.SetLayers(AValue: TArchLayerArray);
begin
  if FLayers=AValue then Exit;
  FLayers:=AValue;
end;

function TArchLayers.GetLayersCount: integer;
begin
  Result:= Length(FLayers);
end;

function TArchLayers.GetLayer(Index: Int64): TArchLayer;
begin
  try
    Result:=FLayers[Index];
  except
    Result:= nil;
  end;
end;

function TArchLayers.GetLayerByName(ALayerName: string): TArchLayer;
var
  i:integer;
begin
  try
    for i:= 0 to Length(FLayers)-1 do
    begin
       if FLayers[i].LayerName= ALayerName then
         Result:= FLayers[i];
    end;
  except
    Result:= nil;
  end;
end;

procedure TArchLayers.SetLayer(Index: Int64; AValue: TArchLayer);
begin
  try
    FLayers[Index]:= AValue;
  except
    ShowMessage('Camada Inexistente"');
    Abort;
  end;
end;

procedure TArchLayers.LoadData(AParameters: TArchParamList);
var
  i:integer;
  endEntity:Boolean;
begin
  SetLength(FLayers,1);
  FLayers[0]:= TArchLayer.Create();
  FLayers[0].LayerName:='_Camada OpenArch';
  FLayers[0].Locked:= True;
  FLayers[0].Visible:= True;
  FLayers[0].Printable:= True;
  for i:= 0 to Length(AParameters)-1 do
  begin
    case AParameters[i].ParamName of
      ARCH_STARTDATA  :
        begin
         if not (AParameters[i].Value=ARCH_ENDENTITY) then
         begin
           SetLength(FLayers,Length(FLayers)+1);
           FLayers[Length(FLayers)-1]:= TArchLayer.Create;
         end;
        end;
      ARCH_LAYER_NAME : FLayers[Length(FLayers)-1].LayerName:= AParameters[i].Value;
      ARCH_LOCKED     :
        begin
         if AParameters[i].Value = 'T' then FLayers[Length(FLayers)-1].Locked:= true
           else FLayers[Length(FLayers)-1].Locked:= false;
        end;
      ARCH_VISIBLE    :
        begin
         if AParameters[i].Value = 'T' then FLayers[Length(FLayers)-1].Visible:= true
           else FLayers[Length(FLayers)-1].Visible:= false;
        end;
      ARCH_PRINTABLE  :
        begin
         if AParameters[i].Value = 'T' then FLayers[Length(FLayers)-1].Printable:= true
           else FLayers[Length(FLayers)-1].Printable:= false;
        end;
    end;
  end;
end;

procedure TArchLayers.AddLayer(ALayer: TArchLayer);
begin
  SetLength(FLayers,Length(FLayers)+1);
  FLayers[Length(FLayers)-1]:= ALayer;
end;

procedure TArchLayers.DelLayer(ALayerName: string);
var
  i:integer;
  layAux:TArchLayerArray;
begin
  SetLength(layAux,0);
  for i:=0 to LayersCount-1 do
  begin
    if not (FLayers[i].LayerName=ALayerName)then
    begin
      SetLength(layAux,Length(layAux)+1);
      layAux[Length(layAux)-1]:= FLayers[i];
    end else
    begin
      FreeAndNil(FLayers[i]);
    end;
  end;
  SetLength(FLayers,Length(layAux));
  for i:=0 to Length(layAux)-1 do
  begin
    FLayers[i]:= layAux[i];
  end;
end;

procedure TArchLayers.EditLayer(ALayerOldName, ALayerNewName, ALocked,
  AVisibles, APrintable: string);
var
  vLayer: TArchLayer;
begin
  TrueBoolStrs:= ['T'];
  FalseBoolStrs:=['F'];
  vLayer:= LayerByName[ALayerOldName];
  vLayer.LayerName:=ALayerNewName;
  vLayer.Locked:=StrToBoolDef(ALocked,True);
  vLayer.Visible:=StrToBoolDef(AVisibles,True);
  vLayer.Printable:=StrToBoolDef(APrintable,True);
end;

procedure TArchLayers.Clone(ALayers: TArchLayers);
var
  i:integer;
begin
  SetLength(Self.FLayers,0);
  if Assigned(ALayers) then
  begin
    for i:= 0 to ALayers.LayersCount-1 do
    begin
     SetLength(Self.FLayers,i+1);
     Self.FLayers[i]:= TArchLayer.Create(TArchLayers(ALayers).FLayers[i].LayerName,
                                         TArchLayers(ALayers).FLayers[i].Locked,
                                         TArchLayers(ALayers).FLayers[i].Visible,
                                         TArchLayers(ALayers).FLayers[i].Printable);
    end;
  end;
end;

{---------------------------------------------------------------------------
                          TArchLayersGroup
----------------------------------------------------------------------------}

procedure TArchLayersGroup.SetLayerGroupName(AValue: String);
begin
  if FLayerGroupName=AValue then Exit;
  FLayerGroupName:=AValue;
end;

procedure TArchLayersGroup.SetLockeds(AValue: TStringList);
begin
  if FLockeds=AValue then Exit;
  FLockeds:=AValue;
end;

procedure TArchLayersGroup.SetPrintables(AValue: TStringList);
begin
  if FPrintables=AValue then Exit;
  FPrintables:=AValue;
end;

procedure TArchLayersGroup.SetVisibles(AValue: TStringList);
begin
  if FVisibles=AValue then Exit;
  FVisibles:=AValue;
end;

constructor TArchLayersGroup.Create();
begin
  FLockeds:= TStringList.Create;
  FVisibles:= TStringList.Create;
  FPrintables:= TStringList.Create;
end;

destructor TArchLayersGroup.Destroy();
begin
  FLockeds.Free;
  FVisibles.Free;
  FPrintables.Free;
end;

procedure TArchLayersGroup.DelAllLockeds();
begin
  FLockeds.Clear;
end;

procedure TArchLayersGroup.DelAllVisibles();
begin
  FVisibles.Clear;
end;

procedure TArchLayersGroup.DelAllPrintables();
begin
  FPrintables.Clear;
end;

procedure TArchLayersGroup.Clone(ALayerGroup: TArchLayersGroup);
var i:integer;
begin
  if not Assigned(Self.FLockeds) then Self.FLockeds:= TStringList.Create;
  if not Assigned(Self.FVisibles) then Self.FVisibles:= TStringList.Create;
  if not Assigned(Self.FPrintables) then Self.FPrintables:= TStringList.Create;

  if Assigned(ALayerGroup) then
  begin
    Self.FLayerGroupName:= TArchLayersGroup(ALayerGroup).FLayerGroupName;
    Self.FLockeds.Clear;
    Self.FVisibles.Clear;
    Self.FPrintables.Clear;
    for i:= 0 to ALayerGroup.Lockeds.Count-1 do
    begin
      Self.Lockeds.Add(ALayerGroup.Lockeds.Strings[i]);
    end;
    for i:= 0 to ALayerGroup.Visibles.Count-1 do
    begin
      Self.Visibles.Add(ALayerGroup.Visibles.Strings[i]);
    end;
    for i:= 0 to ALayerGroup.Printables.Count-1 do
    begin
      Self.Printables.Add(ALayerGroup.Printables.Strings[i]);
    end;
  end;
end;

{---------------------------------------------------------------------------
                          TArchLayer
----------------------------------------------------------------------------}
procedure TArchLayer.SetLayerName(AValue: string);
begin
  if FLayerName=AValue then Exit;
  FLayerName:=AValue;
end;

procedure TArchLayer.SetLocked(AValue: Boolean);
begin
  if FLocked=AValue then Exit;
  FLocked:=AValue;
end;

procedure TArchLayer.SetPrintable(AValue: Boolean);
begin
  if FPrintable=AValue then Exit;
  FPrintable:=AValue;
end;

procedure TArchLayer.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

constructor TArchLayer.Create(AName: string; Alock, AVisble, APrint: Boolean);
begin
  FLayerName:=AName;
  FLocked:=Alock;
  FVisible:=AVisble;
  FPrintable:=APrint;
end;

end.

