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

History:
  2020 12 20 - ET - Start Unit}

unit OpenArchStory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //GLScene:
  GLScene, GLObjects,
  // OpenArch:
  OpenArchTranslate, OpenArchCamera, OpenArchTypes, OpenArchWall,
  OpenArchSysParameters;

type

//Forward Declarations:
  TArchSceneStories = class;

{---------------------------------------------------------------------------
                          TArchStory
----------------------------------------------------------------------------}
  TArchStory = class
    private
      FNumStory:Int64;
      FStoryID,FStoryName: string;
      FHeight,
      FElevation: Double;
      FShowInSectionAndElevation,
      FRepetitive,FEnabled: Boolean;
      FNumOfRepetitions:int64;
      procedure SetElevation(AValue: Double);
      procedure SetEnabled(AValue: Boolean);
      procedure SetHeight(AValue: Double);
      procedure SetNumOfRepetitions(AValue: Int64);
      procedure SetNumStory(AValue: Int64);
      procedure SetRepetitive(AValue: Boolean);
      procedure SetShowInSectionAndElevation(AValue: Boolean);
      procedure SetStoryID(AValue: string);
      procedure SetStoryName(AValue: String);
    public
      constructor Create();
      constructor Create(AElevation, AHeight: Double; AName:String); overload;
      function GetNewID(Prefix:String):string;
      procedure SetStory(AElevation, AHeight: Double; AName: String);
      function SendByMessage():TArchMessage;
      property StoryID:string read FStoryID write SetStoryID;
      property NumStory:Int64 read FNumStory write SetNumStory;
      property StoryName: String read FstoryName write SetStoryName;
      property ShowInSectionAndElevation:Boolean read FShowInSectionAndElevation write SetShowInSectionAndElevation;
      property Height:Double read FHeight write SetHeight;
      property Elevation:Double read FElevation write SetElevation;
      property Repetitive:Boolean read FRepetitive write SetRepetitive;
      property NumOfRepetitions:Int64 read FNumOfRepetitions write SetNumOfRepetitions;
      property Enabled:Boolean read FEnabled write SetEnabled;
  end;

{---------------------------------------------------------------------------
                          TArchStoryArray
----------------------------------------------------------------------------}
  TArchStoryArray = array of TArchStory;

{---------------------------------------------------------------------------
                          TArchStories
----------------------------------------------------------------------------}
  TArchStories = class
    private
      FStories: TArchStoryArray;
      FSceneStoriesPlan,
      FSceneStories3D: TArchSceneStories;
      function GetStoriesCount: Int64;
      function GetStory(Index: Int64): TArchStory;
      function GetStoryByNum(NumOfStory: Int64): TArchStory;
      function GetStoryEnable: TArchStory;
      procedure SetSceneStories3D(AValue: TArchSceneStories);
      procedure SetSceneStoriesPlan(AValue: TArchSceneStories);
      procedure SetStory(Index: Int64; AValue: TArchStory);
      procedure SetStoryEnable(AStoryID:string);
    public
      destructor Destroy; override;
      function GetTopStoryNum:Int64;
      procedure LoadData(AParameters:TArchParamList);
      procedure AddStory(AStory: TArchStory);
      procedure DelStory(AStoryID: string);
      procedure RefreshStories();
      procedure SetEnable(ANumOfStory:Integer);
      property Story[Index:Int64]:TArchStory read GetStory write SetStory;
      property StoryByNum[NumOfStory:Int64]:TArchStory read GetStoryByNum;
      property StoriesCount:Int64 read GetStoriesCount;
      property SceneStoriesPlan:TArchSceneStories read FSceneStoriesPlan write SetSceneStoriesPlan;
      property SceneStories3D:TArchSceneStories read FSceneStories3D write SetSceneStories3D;
      property StoryEnable:TArchStory read GetStoryEnable;
    end;

  {---------------------------------------------------------------------------
                                TArchSceneStory
  ----------------------------------------------------------------------------}
  TArchSceneStory = class(TArchEntity)
    private
      FStoryName,FStoryID: String;
      FHeight,
      FElevation: Double;
      FCamera: TArchCamera;
      FEnabled:Boolean;
      procedure AddCamera();
      procedure AddWalls();
      function GetWalls: TArchWalls;
      function GetMessageToSend(var AMessage:TArchMessage): TArchMessage;
      procedure SetElevation(AValue: Double);
      procedure SetEnabled(AValue: Boolean);
      procedure SetHeight(AValue: Double);
      procedure SetStoryID(AValue: string);
    public
      procedure AddZeroPoint();
      function ChangeState(AMessage:TArchMessage):TArchMessage;
      constructor Create(AOwner: TComponent; AStoryData: TArchMessage); overload;
      procedure Update(AStory:TArchSceneStory; AMessage: TArchMessage);
      property Camera: TArchCamera read FCamera;
      property Walls: TArchWalls read GetWalls;
      property StoryName:string read FStoryName write FStoryName;
      property Elevation:Double read FElevation write SetElevation;
      property Height:Double read FHeight write SetHeight;
      property StoryID:string read FStoryID write SetStoryID;
      property Enabled:Boolean read FEnabled write SetEnabled;
  end;

  {---------------------------------------------------------------------------
                              TArchSceneStories
  ----------------------------------------------------------------------------}
  TArchSceneStories = class(TArchEntity)
    private
      FEnabledSceneStory: TArchSceneStory;
      function GetSceneStoriesCount: Int64;
      function GetEnableSceneStory:TArchSceneStory;
    public
      procedure SetEnabledStory(AStoryID:String);
      function GetEnableSceneStory(AStoryName:string): TArchSceneStory;
      constructor Create(AOwner: TComponent; AEnviroment: TArchEnviromentType); overload;
      function FindStory(AStoryName: string):TArchSceneStory;
      function ChangeState(AMessage:TArchMessage):TArchMessage;
      {This method update the objects in the 2d/3d buildings}
      procedure Update(ASceneStory: TArchSceneStories; AMEssage: TArchMessage);
      procedure AddStory(AMessage:TArchMessage);
      procedure DelStory(AStoryID:string);
      property EnabledSceneStory:TArchSceneStory read GetEnableSceneStory;
      property EnviromentType: TArchEnviromentType read FEnviromentType;
      property SceneStoriesCount:Int64 read GetSceneStoriesCount;
  end;


implementation

{---------------------------------------------------------------------------
                          TArchStories
----------------------------------------------------------------------------}

function TArchStories.GetStory(Index: Int64): TArchStory;
begin
  try
    Result:=FStories[Index];
  except
    Result:= nil;
  end;
end;

function TArchStories.GetStoryByNum(NumOfStory: Int64): TArchStory;
var
  i: Integer;
begin
  try
    for i:= 0 to StoriesCount-1 do
    begin
      if Story[i].NumStory= NumOfStory then
        begin
          Result:=Story[i];
          Break;
        end;
    end;
  except
    Result:= nil;
  end;
end;

function TArchStories.GetStoryEnable: TArchStory;
var i:integer;
begin
  for i:= 0 to StoriesCount-1 do
  begin
    if Story[i].Enabled= True then
      begin
        Result:=Story[i];
        Break;
      end;
  end;
end;

procedure TArchStories.SetStoryEnable(AStoryID: string);
var i:integer;
begin
  for i:=0 to StoriesCount-1 do
  begin
    if Story[i].StoryID=AStoryID then
    begin
      Story[i].Enabled:=true;
    end else
    begin
      Story[i].Enabled:=False;
    end;
  end;
  SceneStories3D.SetEnabledStory(AStoryID);
  SceneStoriesPlan.SetEnabledStory(AStoryID);
end;

procedure TArchStories.SetSceneStories3D(AValue: TArchSceneStories);
begin
  if FSceneStories3D=AValue then Exit;
  FSceneStories3D:=AValue;
end;

procedure TArchStories.SetSceneStoriesPlan(AValue: TArchSceneStories);
begin
  if FSceneStoriesPlan=AValue then Exit;
  FSceneStoriesPlan:=AValue;
end;

function TArchStories.GetStoriesCount: Int64;
begin
  Result:= Length(FStories);
end;

procedure TArchStories.SetStory(Index: Int64; AValue: TArchStory);
begin
  try
    FStories[Index]:= AValue;
  except
    ShowMessage('Pavimento Inexistente');
    Abort;
  end;
end;

destructor TArchStories.Destroy;
var
  i:Integer;
begin
  for i:= Length(FStories)-1 to 0 do
  begin
    FreeAndNil(FStories[i]);
    SetLength(FStories,i);
  end;
  inherited Destroy;
end;

function TArchStories.GetTopStoryNum: Int64;
var
  i, j, vTop:integer;
begin
  vTop:=0;
  for i:=0 to StoriesCount-1 do
  begin
    j:=Story[i].NumStory;
    if j > vTop then vTop:= j;
  end;
  Result:= vTop;
end;

procedure TArchStories.LoadData(AParameters: TArchParamList);
var
  i:Int64;
  vStory:TArchStory; //Testando Alternativa
begin
  DecimalSeparator:='.';
  TrueBoolStrs:=['T'];
  FalseBoolStrs:=['F'];
  for i:=0 to Length(AParameters)-1 do
  begin
    case AParameters[i].ParamName of
      ARCH_STARTDATA:
        begin
         if not (AParameters[i].Value=ARCH_ENDENTITY) then
         begin
           vStory:=TArchStory.Create();
         end else
         begin
            AddStory(vStory);
         end;
        end;
      ARCH_STORY_ID   : vStory.StoryID:= AParameters[i].Value;
      ARCH_STORY_NUM  : vStory.NumStory:= StrToInt(AParameters[i].Value);
      ARCH_STORY_NAME : vStory.StoryName:= AParameters[i].Value;
      ARCH_STORY_ELEVATION : vStory.Elevation:= StrToFloat(AParameters[i].Value);
      ARCH_STORY_HEIGHT : vStory.Height:= StrToFloat(AParameters[i].Value);
      ARCH_STORY_REPEAT : vStory.Repetitive:=StrToBoolDef(AParameters[i].Value,True);
      ARCH_STORY_NUMOFREPETITIONS : vStory.NumOfRepetitions:= StrToInt(AParameters[i].Value);
      ARCH_STORY_SHOWINSECELEVATION : vStory.ShowInSectionAndElevation:=StrToBoolDef(AParameters[i].Value,True);
      ARCH_ENABLED : vStory.Enabled:= StrToBoolDef(AParameters[i].Value,False);
    end;
  end;
  DecimalSeparator:=',';
end;

procedure TArchStories.AddStory(AStory: TArchStory);
begin
  SetLength(FStories,Length(FStories)+1);
  FStories[Length(FStories)-1]:= AStory;
  SceneStoriesPlan.AddStory(FStories[Length(FStories)-1].SendByMessage());
  SceneStories3D.AddStory(FStories[Length(FStories)-1].SendByMessage());
end;

procedure TArchStories.DelStory(AStoryID: string);
var
  staux: TArchStoryArray;
  storyIdEnable:string;
  i:integer;
begin
  SetLength(staux,0);
  for i:=0 to StoriesCount-1 do
  begin
    if not (FStories[i].StoryID=AStoryID)then
    begin
      SetLength(staux,Length(staux)+1);
      staux[Length(staux)-1]:= FStories[i];
      storyIdEnable:=FStories[i].StoryID;
    end
    else
    begin
      if (i=0) and (FStories[i].Enabled) then SetStoryEnable(FStories[i+1].StoryID)
        else if (FStories[i].Enabled) then SetStoryEnable(storyIdEnable);
      FreeAndNil(FStories[i]);
    end;
  end;
  SetLength(FStories,Length(staux));
  for i:=0 to Length(staux)-1 do
  begin
    FStories[i]:= staux[i];
  end;
  SceneStoriesPlan.DelStory(AStoryID);
  SceneStories3D.DelStory(AStoryID);
end;

procedure TArchStories.RefreshStories();
var
  i,j:integer;
  vChecked:Boolean;
  sceneStory: TArchSceneStory;
begin
  //Make the stories deletes:
  for i:=SceneStoriesPlan.Count-1 downto 0 do
  begin
    vChecked:=False;
    sceneStory:= SceneStoriesPlan.Children[i] as TArchSceneStory;
    for j:=0 to StoriesCount -1 do
    begin
      if (Story[j].StoryID=sceneStory.StoryID) then
      begin
        vChecked:=True;
        ShowMessage(sceneStory.StoryID+' Mantem Piso');
      end;
    end;
    if not vChecked then
    begin
      ShowMessage(sceneStory.StoryID+' Deletando o Piso');
      SceneStoriesPlan.DelStory(sceneStory.StoryID);
    end;
  end;
end;

procedure TArchStories.SetEnable(ANumOfStory: Integer);
var
  i:integer;
begin
  SetStoryEnable(StoryByNum[ANumOfStory].StoryID);
end;

{---------------------------------------------------------------------------
                          TArchStory
----------------------------------------------------------------------------}
function TArchStory.GetNewID(Prefix:String):string;
var
  reg:array [1..7] of word;
  Ano,Mes,Dia,Hora,Minuto,Segundo,MSeg,i:word;
begin
  Result:='';
  DecodeDate(Date,reg[1],reg[2],reg[3]);
  Ano:=StrToInt(Copy(IntToStr(reg[1]),3,2));
  DecodeTime(Time,reg[4],reg[5],reg[6],reg[7]);
  randomize;
  for i:=1 to 7 do
     begin
          reg[i]:=reg[i]+Random(100);
          Result:=Result+IntToHex(reg[i],2);
     end;
  Result:=Prefix+Result;
end;

procedure TArchStory.SetStory(AElevation, AHeight: Double; AName: String);
begin
  FStoryName:=AName;
  FHeight:=AHeight;
  FElevation:=AElevation;
end;

function TArchStory.SendByMessage(): TArchMessage;
begin
  SetLength(Result,6);
  with Result[0] do
  begin
    ParamName:=ARCH_ENTITY;
    ParamType:=3;
    ParamStrValue:=ARCH_STORY;
  end;
  with Result[1] do
  begin
    ParamName:=ARCH_NAME;
    ParamType:=3;
    ParamStrValue:=StoryName;
  end;
  with Result[2] do
  begin
    ParamName:=ARCH_STORY_ELEVATION;
    ParamType:=2;
    ParamFltValue:=FElevation;
  end;
  with Result[3] do
  begin
    ParamName:=ARCH_STORY_HEIGHT;
    ParamType:=2;
    ParamFltValue:=FHeight;
  end;
  with Result[4] do
  begin
    ParamName:=ARCH_STORY_ID;
    ParamType:=3;
    ParamStrValue:=FStoryID;
  end;
  with Result[5] do
  begin
    ParamName:=ARCH_ENABLED;
    ParamType:=3;
    ParamBolValue:=FEnabled;
  end;
end;

procedure TArchStory.SetElevation(AValue: Double);
begin
  if FElevation=AValue then Exit;
  FElevation:=AValue;
end;

procedure TArchStory.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
end;

procedure TArchStory.SetHeight(AValue: Double);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TArchStory.SetNumOfRepetitions(AValue: Int64);
begin
  if FNumOfRepetitions=AValue then Exit;
  FNumOfRepetitions:=AValue;
end;

procedure TArchStory.SetNumStory(AValue: Int64);
begin
  if FNumStory=AValue then Exit;
  FNumStory:=AValue;
end;

procedure TArchStory.SetRepetitive(AValue: Boolean);
begin
  if FRepetitive=AValue then Exit;
  FRepetitive:=AValue;
end;

procedure TArchStory.SetShowInSectionAndElevation(AValue: Boolean);
begin
  if FShowInSectionAndElevation=AValue then Exit;
  FShowInSectionAndElevation:=AValue;
end;

procedure TArchStory.SetStoryID(AValue: string);
begin
  if FStoryID=AValue then Exit;
  FStoryID:=AValue;
end;

procedure TArchStory.SetStoryName(AValue: String);
begin
  if FstoryName=AValue then Exit;
  FstoryName:=AValue;
end;

constructor TArchStory.Create();
begin

end;

constructor TArchStory.Create(AElevation, AHeight: Double;
  AName: String);
begin
  FStoryID:= GetNewID('Story');
  FHeight:= AHeight;
  FElevation:= AElevation;
  FStoryName:= AName;
end;



{---------------------------------------------------------------------------
                          TArchSceneStories
----------------------------------------------------------------------------}

procedure TArchSceneStories.SetEnabledStory(AStoryID: String);
var
  i:integer;
  vSceneStory:TArchSceneStory;
begin
  for i:=0 to Count-1 do
  begin
    vSceneStory:= Children[i] as TArchSceneStory;
    if vSceneStory.StoryID = AStoryID then
    begin
      vSceneStory.Enabled:=True;
    end else
    begin
      vSceneStory.Enabled:=False;
    end;
  end;
end;

function TArchSceneStories.GetSceneStoriesCount: Int64;
var i,stories:integer;
begin
  stories:=0;
  for i:=0 to Count-1 do
  begin
    if Children[i] is TArchSceneStory then
    begin
      stories:=stories+1;
    end;
  end;
  Result:= stories;
end;

function TArchSceneStories.GetEnableSceneStory: TArchSceneStory;
var
  i:integer;
  vSceneStory: TArchSceneStory;
begin
  for i:= 0 to Count-1 do
  begin
    vSceneStory:= Children[i] as TArchSceneStory;
    if vSceneStory.Enabled then
    begin
      Result:= vSceneStory;
      Break;
    end;
  end;
end;

constructor TArchSceneStories.Create(AOwner: TComponent;
  AEnviroment: TArchEnviromentType);
begin
  inherited Create(AOwner, ARCH_STORIES);
  {Although the ancestral class is named ARCH_STORIES in its creation method,
  it is important to keep Name = ARCHSTORIES in this constructor, because the
  GetSceneStories Method, in the TArchBuilding class, searches for SceneStories
  by name.}
  FEnviromentType:=AEnviroment;
  Name:=ARCH_STORIES;
end;

function TArchSceneStories.FindStory(AStoryName: string): TArchSceneStory;
var
  i: integer;
  vStory: TArchSceneStory;
begin
  i:=0;
  vStory:= Children[i] as TArchSceneStory;
  while vStory.StoryName <> AStoryName do
  begin
    vStory:= Children[i] as TArchSceneStory;
    i:= i+1;
  end;
  Result:= vStory;
end;

{This function serves to transmit information by generic TArchMessage structure.
This informatio must be organized in the following order:
Parameter 0 = TArchProjectState - That indicates change of the states
Parameter 1 = TArchElementType - that indicates the element processed
Parameter 2 = TArchContextInfo - Containing information about the viewer
Parameter 3 = TArchEnviromentType - Containing the type of enviroment.
----------
Here is directed to the Enabled Story
}
function TArchSceneStories.ChangeState(AMessage: TArchMessage): TArchMessage;
begin
  Result:= EnabledSceneStory.ChangeState(AMessage);
end;

procedure TArchSceneStories.Update(ASceneStory: TArchSceneStories;
  AMEssage: TArchMessage);
var
  vStory,vStorySent:TArchSceneStory;
begin
  {Looking for the Story that was updated and sent by AMessage:}
  vStory:=FindStory(AMEssage[5].ParamStrValue);
  {Locating the Story in Scenestory Sent}
  vStorySent:= ASceneStory.FindStory(AMEssage[5].ParamStrValue);
  {Updating the current Story with the Story submitted}
  vStory.Update(vStorySent,AMEssage);
end;

procedure TArchSceneStories.AddStory(AMessage: TArchMessage);
begin
  AddChild(TArchSceneStory.Create(Self,AMessage));
end;

procedure TArchSceneStories.DelStory(AStoryID: string);
var
  i:integer;
  sceneStory: TArchSceneStory;
begin
  for i:=0 to Count-1 do
  begin
    sceneStory:= Children[i] as TArchSceneStory;
    if (sceneStory.StoryID=AStoryID) then
    begin
      FreeAndNil(sceneStory);
    end;
  end;
end;

function TArchSceneStories.GetEnableSceneStory(AStoryName: string
  ): TArchSceneStory;
begin
  Result:= FEnabledSceneStory;
end;

{---------------------------------------------------------------------------
                          TArchSceneStory
----------------------------------------------------------------------------}
procedure TArchSceneStory.AddCamera();
begin
  AddChild(TArchCamera.Create(Self));
  FCamera:= Children[0] as TArchCamera;
  with FCamera do
  begin
    CameraName:=StoryName+'Camera';
    CameraStyle:=csOrthogonal;
    DepthOfView:=10;
    Direction.SetVector(0,0,-1);
    FocalLength:=1;
    KeepFOVMode:=ckmHorizontalFOV;
    NearPlaneBias:=0.2;
    Position.SetPoint(0,0,FElevation+FHeight);
    SceneScale:=1;
    Up.SetVector(0,1,0);
  end;
end;

procedure TArchSceneStory.AddWalls();
begin
  AddChild(TArchWalls.Create(Self, FEnviromentType));
end;

function TArchSceneStory.GetWalls: TArchWalls;
var
  i: integer;
  entity: TArchEntity;
begin
  // Here we start countig from 1, as the first position (0)
  // is reserved for the camera.
  i:=1;
  entity:= Children[i] as TArchEntity;
  while entity.EntityName <> ARCH_WALLS do
  begin
    i:= i+1;
    entity:= Children[i] as TArchEntity;
  end;
  Result:= entity as TArchWalls;
end;

{This function add the Name of Story in the return Message}
function TArchSceneStory.GetMessageToSend(var AMessage: TArchMessage
  ): TArchMessage;
begin
  SetLength(Result,6);
  Result[0]:= AMessage[0];
  Result[1]:= AMessage[1];
  Result[2]:= AMessage[2];
  Result[3]:= AMessage[3];
  Result[4]:= AMessage[4];
  with Result[5] do
  begin
    ParamName:=ARCH_NAME;
    ParamType:= 3;
    ParamStrValue:=StoryName;
  end;
end;

procedure TArchSceneStory.SetElevation(AValue: Double);
begin
  if FElevation=AValue then Exit;
  FElevation:=AValue;
end;

procedure TArchSceneStory.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
end;

procedure TArchSceneStory.SetHeight(AValue: Double);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TArchSceneStory.SetStoryID(AValue: string);
begin
  if FStoryID=AValue then Exit;
  FStoryID:=AValue;
end;

procedure TArchSceneStory.AddZeroPoint();
var
  vPoint: TGLPoints;
begin
  vPoint:=TGLPoints.Create(Self);
  with vPoint do
  begin
    Colors.Add(0,0,0,1);
    Size:=3.5;
    Style:=psRound;
    Static:=true;
    Position.Z:= FElevation;
  end;
  AddChild(vPoint);
end;

{This function serves to transmit information by generic TArchMessage structure.
This informatio must be organized in the following order:
Parameter 0 = TArchProjectState - That indicates change of the states
Parameter 1 = TArchElementType - that indicates the element processed
Parameter 2 = TArchContextInfo - Containing information about the viewer
Parameter 3 = TArchEnviromentType - Containing the type of enviroment.
If the returnMessage is after finishing the element, it will load the 05th
parameter:
Parameter 4 = Element ID (string);
And add the 06th parameter, the Stor ID:
Parameter 5 = Story ID (string)
----------
Here is directed to the management of each element:
}
function TArchSceneStory.ChangeState(AMessage: TArchMessage): TArchMessage;
var
  returnMessage: TArchMessage;
begin
  case AMessage[1].ParamETValue of
    etWall:
      begin
        returnMessage:= Walls.ChangeState(AMessage);
        if returnMessage[0].ParamPSValue= psFinishingElement then
        begin
          returnMessage:= GetMessageToSend(returnMessage);
          Result:= returnMessage;
        end else
        begin
          Result:= returnMessage;
        end;
      end;
    etStandBy:
      begin
        returnMessage:= AMessage;
      end;
  end;
end;

constructor TArchSceneStory.Create(AOwner: TComponent; AStoryData: TArchMessage);
var
  vSceneStories:TArchSceneStories;
begin
  inherited Create(AOwner,ARCH_STORY);
  if (AStoryData[0].ParamName= ARCH_ENTITY) and (AStoryData[0].ParamStrValue= ARCH_STORY) then
  begin
    if AStoryData[1].ParamName= ARCH_NAME then
      StoryName:=AStoryData[1].ParamStrValue;
    if AStoryData[2].ParamName= ARCH_STORY_ELEVATION then
      FElevation:=AStoryData[2].ParamFltValue;
    if AStoryData[3].ParamName= ARCH_STORY_HEIGHT then
      FHeight:=AStoryData[3].ParamFltValue;
    if AStoryData[4].ParamName= ARCH_STORY_ID then
      StoryID:=AStoryData[4].ParamStrValue;
    if AStoryData[5].ParamName= ARCH_ENABLED then
      Enabled:=AStoryData[5].ParamBolValue;
  end;
  vSceneStories:= AOwner as TArchSceneStories;
  SetEnviroment(vSceneStories.FEnviromentType);
  {The childrens first position should be reserved for the camera}
  AddCamera();
  AddWalls();
  AddZeroPoint();
end;

procedure TArchSceneStory.Update(AStory: TArchSceneStory;
  AMessage: TArchMessage);
begin
  {Directing the Update according to the type of the element:}
  case AMessage[1].ParamETValue of
    etWall: Walls.Update(AStory.Walls,AMessage);
  end;
end;

end.

