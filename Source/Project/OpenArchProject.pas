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
 2016 09 20 - ET - Unit implementation Start's.
}

unit OpenArchProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls,
  //Glscene:
  GLScene, GLObjects, OpenGLTokens, GLGraph, GLColor, GLVectorGeometry,
  //Openarch:
  OpenArchTranslate, OpenArchTypes, OpenArchStory, OpenArchProjectInfo,
  OpenArchGraphicResources,
  OpenArchCamera, OpenArchLayer, OpenArchIOFile, OpenArchSysParameters;

type

{---------------------------------------------------------------------------
                          TArchBuilding
----------------------------------------------------------------------------}

  { TArchBuilding }

  TArchBuilding = class(TGLScene)
    private
      FEnviromentType: TArchEnviromentType;
      function GetCamera: TArchCamera;
      function GetGrid: TGLXYZGrid;
      function GetSceneStories: TArchSceneStories;
      procedure AddCamera();
      procedure AddGrid();
      procedure AddArch3dAxis;
      procedure AddStoriesNode();
      function GetMessageToSend(var AMessage:TArchMessage):TArchMessage;
      function GetEnabledStory:TArchSceneStory;
    public
      constructor Create(AOwner: TComponent; AEnviroment:TArchEnviromentType); overload;
      function ChangeState(var AMessage: TArchMessage):TArchMessage;
      procedure Update(ABuilding:TArchBuilding; AMessage: TArchMessage);
      procedure SetEnabledStory(AStoryID: string);
      property Camera:TArchCamera read GetCamera;
      property SceneStories: TArchSceneStories read GetSceneStories;
      property Grid3d: TGLXYZGrid read GetGrid;
      property EnabledStory: TArchSceneStory read GetEnabledStory;
  end;

{---------------------------------------------------------------------------
                          TArchProject
----------------------------------------------------------------------------}

  { TArchProject }

  TArchProject = class
  private
    FIOFile: TArchIOStreamFile;
    FProjectInfo: TArchProjectInfo;
    FLayers: TArchLayers;
    FLayersSet: TArchLayersSet;
    FStories: TArchStories;
    FGraphicResources: TArchGraphicResources;
    FStatus: TArchProjectState;
    FEnviromentActive: TArchEnviromentType;
    FElementTypeHandled: TArchElementType;
    FBuildingPlan, FBuilding3D: TArchBuilding;
    {}
    function GetProjectState: TArchProjectState;
    function GetStories: TArchStories;
    procedure SetEnviroment(AValue: TArchEnviromentType);
    procedure SetLayers(AValue: TArchLayers);
    procedure SetProjectInfo(AValue: TArchProjectInfo);
  public
    constructor Create(AOwner: TComponent; AFilePath: String);
    procedure LoadData();
    destructor Destroy; override;
    procedure ChangeState(AMessage: TArchMessage);
    procedure CreateBuilding(AOwner: TComponent);
    procedure SetEnabledStory();
    property Stories: TArchStories read GetStories;
    property Status:TArchProjectState read GetProjectState;
    property ElementHandled: TArchElementType read FElementTypeHandled;
    property Enviroment: TArchEnviromentType read FEnviromentActive write SetEnviroment;
    property Building3D: TArchBuilding read FBuilding3D;
    property BuildingPlan: TArchBuilding read FBuildingPlan;
    property ProjectInfo:TArchProjectInfo read FProjectInfo write SetProjectInfo;
    property Layers:TArchLayers read FLayers write SetLayers;
    property LayersSet:TArchLayersSet read FLayersSet;
    property GraphicResources:TArchGraphicResources read FGraphicResources;
  end;

{======================================================================================}
implementation
{======================================================================================}

{---------------------------------------------------------------------------
                          TArchBuilding
----------------------------------------------------------------------------}
function TArchBuilding.GetSceneStories: TArchSceneStories;
begin
  Result:= Objects.FindChild(ARCH_STORIES,true) as TArchSceneStories;
end;

function TArchBuilding.GetCamera: TArchCamera;
var
  vEnablescStory: TArchSceneStory;
begin
  if (FEnviromentType=et3d) then
  begin
    Result:=Objects.FindChild(ARCH_3DCAMERA,true) as TArchCamera;
  end;
  if (FEnviromentType=etFloorPlan) then
  begin
    vEnablescStory:=SceneStories.EnabledSceneStory;
    Result:= vEnablescStory.Camera;
  end;
end;

function TArchBuilding.GetGrid: TGLXYZGrid;
begin
  if (FEnviromentType=et3d) then
  begin
    Result:=Objects.FindChild(ARCH_3DGRID,true)as TGLXYZGrid;
  end;
end;

procedure TArchBuilding.AddCamera();
var
  vcamera: TArchCamera;
  vLight: TGLLightSource;
  dumCam: TGLDummyCube;
begin
  Objects.AddNewChild(TArchCamera);
  vcamera:=Objects.Children[Objects.Count-1] as TArchCamera;
  vcamera.Name:=ARCH_3DCAMERA;
  Objects.AddNewChild(TGLDummyCube);
  dumCam:= Objects.Children[Objects.Count-1] as TGLDummyCube;
  dumCam.Position.SetPoint(0,0,0);
  with vCamera do
  begin
    CameraStyle:=csPerspective;
    DepthOfView:=200;
    FocalLength:=20;
    KeepFOVMode:=ckmHorizontalFOV;
    NearPlaneBias:=1;
    SceneScale:=1;
    AddChild(TGLLightSource.Create(Self));
    vLight:= Children[0] as TGLLightSource;
    TargetObject:= dumCam;
  end;
  with vLight do
  begin
    Ambient.Color:= clrGray90;
    ConstAttenuation:=1;
    Diffuse.Color:= clrWhite;
    LightStyle:= lsSpot;
  end;
end;

procedure TArchBuilding.AddGrid();
var
   vgrid: TGLXYZGrid;
begin
  Objects.AddNewChild(TGLXYZGrid);
  vgrid:=Objects.Children[Objects.Count-1] as TGLXYZGrid;
  vgrid.Name:=ARCH_3DGRID;
  with vgrid do
  begin
    AntiAliased:=true;
    LineColor.Color:= clrActiveCaption;
    LinesStyle:= glsLine;
    LineWidth:=0.1;
    Parts:=[gpX,gpY];
    Pickable:=False;
    Position.SetPoint(-5,-5,0);
    with XSamplingScale do
    begin
      Max:=10;
      Min:=0;
      Origin:=0;
      Step:=0.5;
    end;
    with YSamplingScale do
    begin
      Max:=10;
      Min:=0;
      Origin:=0;
      Step:=0.5;
    end;
  end;
end;

procedure TArchBuilding.AddArch3dAxis;
var
   vAxisX, vAxisY, vAxisZ: TGLLines;
begin
  Objects.AddNewChild(TGLLines);
  vAxisX:= Objects.Children[Objects.Count-1] as TGLLines;
  Objects.AddNewChild(TGLLines);
  vAxisY:= Objects.Children[Objects.Count-1] as TGLLines;
  Objects.AddNewChild(TGLLines);
  vAxisZ:= Objects.Children[Objects.Count-1] as TGLLines;
  with vAxisX do
  begin
   with Nodes do
   begin
     AddNode(0,0,0);
     AddNode(1,0,0);
   end;
   LineColor.Color:= clrBlack;
   LineWidth:=3;
   Pickable:=False;
   NodesAspect:= lnaInvisible;
  end;
  with vAxisY do
  begin
   with Nodes do
   begin
     AddNode(0,0,0);
     AddNode(0,1,0);
   end;
   LineColor.Color:= clrBlack;
   LineWidth:=3;
   Pickable:=False;
   NodesAspect:= lnaInvisible;
  end;
  with vAxisZ do
  begin
   with Nodes do
   begin
     AddNode(0,0,0);
     AddNode(0,0,1);
   end;
   LineColor.Color:= clrBlack;
   LineWidth:=3;
   Pickable:=False;
   NodesAspect:= lnaInvisible;
  end;
end;

procedure TArchBuilding.AddStoriesNode();
begin
  Objects.AddChild(TArchSceneStories.Create(Self,FEnviromentType));
end;

procedure TArchBuilding.SetEnabledStory(AStoryID: string);
begin
  SceneStories.SetEnabledStory(AStoryID);
end;

function TArchBuilding.GetMessageToSend(var AMessage: TArchMessage): TArchMessage;
begin
  if (Length(AMessage)=3) then
  begin
    SetLength(AMessage, 4);
  end;
  AMessage[3].ParamName:=ARCH_ENVIROMENT;
  AMessage[3].ParamType:=8;
  AMessage[3].ParamEVTValue:=FEnviromentType;
  Result:= AMessage;
end;

function TArchBuilding.GetEnabledStory: TArchSceneStory;
begin
  Result:= SceneStories.EnabledSceneStory;
end;

constructor TArchBuilding.Create(AOwner: TComponent;
  AEnviroment: TArchEnviromentType);
begin
  inherited Create(AOwner);
  FEnviromentType:= AEnviroment;
  AddStoriesNode();
  if (FEnviromentType=et3d) then
  begin
    AddCamera();
    AddGrid();
    AddArch3dAxis;
  end;
end;

{This function serves to transmit information by generic TArchMessage structure.
This informatio must be organized in the following order:
Parameter 0 = TArchProjectState - That indicates change of the states
Parameter 1 = TArchElementType - that indicates the element processed
Parameter 2 = TArchContextInfo - Containing information about the viewer
----------
Here the type of Enviroment is included in the last position of the structure.
Parameter 3 = TArchEnviromentType }
function TArchBuilding.ChangeState(var AMessage: TArchMessage):TArchMessage;
var
  vMessage: TArchMessage;
begin
  vMessage:= GetMessageToSend(AMessage);
  Result:= SceneStories.ChangeState(vMessage);
end;

{Here we update SceneStories with the information that comes from 2d / 3d:}
procedure TArchBuilding.Update(ABuilding: TArchBuilding; AMessage: TArchMessage);
begin
  {Sending the scenestories of the sent building to update the scenestoriews
  of the current building}
  SceneStories.Update(ABuilding.SceneStories,AMessage);
end;

{---------------------------------------------------------------------------
                          TArchProject
----------------------------------------------------------------------------}
function TArchProject.GetStories: TArchStories;
begin
  Result:= FStories;
end;

procedure TArchProject.SetEnviroment(AValue: TArchEnviromentType);
begin
  if FEnviromentActive=AValue then Exit;
  FEnviromentActive:=AValue;
end;

procedure TArchProject.SetLayers(AValue: TArchLayers);
begin
  if FLayers=AValue then Exit;
  FLayers:=AValue;
end;

procedure TArchProject.SetProjectInfo(AValue: TArchProjectInfo);
begin
  if FProjectInfo=AValue then Exit;
  FProjectInfo:=AValue;
end;

function TArchProject.GetProjectState: TArchProjectState;
begin
  Result:= FStatus;
end;

constructor TArchProject.Create(AOwner: TComponent; AFilePath: String);
var
  i:integer;
begin
  FStatus:=psStandBy;
  FProjectInfo:= TArchProjectInfo.Create;
  FLayers:= TArchLayers.Create;
  FLayersSet:= TArchLayersSet.Create;
  FGraphicResources:= TArchGraphicResources.Create;
  CreateBuilding(AOwner);
  FIOFile:= TArchIOStreamFile.New(TFileStream.Create(AFilePath,fmOpenReadWrite));
  LoadData();
end;

procedure TArchProject.LoadData();
begin
  with FIOFile do
  begin
    LoadHeader();
    ProjectInfo.LoadData(LoadSection(ARCH_SPROJECTINFO));
    Layers.LoadData(LoadSection(ARCH_SLAYERS));
    LayersSet.LoadData(LoadSection(ARCH_SLAYER_SET));
    Stories.LoadData(LoadSection(ARCH_SSTORIES));
    GraphicResources.LoadData(LoadSection(ARCH_GRAPHIC_RESOURCES));
  end;
end;

destructor TArchProject.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FStories);
  FreeAndNil(FIOFile);
  FreeAndNil(FProjectInfo);
  FreeAndNil(FLayers);
end;

{This function serves to transmit information by generic TArchMessage structure.
This informatio must be organized in the following order:
Parameter 0 = TArchProjectState - That indicates change of the states
Parameter 1 = TArchElementType - that indicates the element processed
Parameter 2 = TArchContextInfo - Containing information about the viewer
}
procedure TArchProject.ChangeState(AMessage: TArchMessage);
var
  returnMessage: TArchMessage;
begin
  if AMessage[0].ParamType=5 then
  begin
    FStatus:= AMessage[0].ParamPSValue;
    FElementTypeHandled:= AMessage[1].ParamETValue;
    case FStatus of
      psStandBy :
        begin
          FElementTypeHandled:= etStandBy;
        end;
      psNewElement, psStartingElement :
        begin
          returnMessage:= BuildingPlan.ChangeState(AMessage);
          FStatus:= returnMessage[0].ParamPSValue;
          if returnMessage[0].ParamPSValue= psFinishingElement then
          begin
            Building3D.Update(BuildingPlan,returnMessage);
          end;
        end;
      psFinishingElement :
        begin
          FStatus:= psStandBy;
          FElementTypeHandled:= etStandBy;
        end;
    end;
  end;
end;

procedure TArchProject.CreateBuilding(AOwner: TComponent);
var
  i,j:integer;
  sceneStories: array [0..1] of TArchSceneStories;
  sc: TArchSceneStory;
begin
  FStories:= TArchStories.Create;
  FBuilding3D:= TArchBuilding.Create(AOwner, et3d);
  FBuildingPlan:= TArchBuilding.Create(AOwner, etFloorPlan);
  Stories.SceneStoriesPlan:= FBuildingPlan.SceneStories;
  Stories.SceneStories3D:= FBuilding3D.SceneStories;
end;

procedure TArchProject.SetEnabledStory();
var
  vBuildings: array [0..1] of TArchBuilding;
  i:integer;
  vEnabledSceneStory: TArchSceneStory;
begin
  vBuildings[0]:= FBuilding3D;
  vBuildings[1]:= FBuildingPlan;
end;


end.

