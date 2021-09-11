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
OpenArchMainForm: Main Form of Application


 2018 05 20 - ET - Unit implementation Start's.
}

unit OpenArchMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, LCLType,
  ComCtrls, ExtCtrls,
  //Openarch:
  OpenArchTranslate,OpenArchProject, OpenArchViewer, OpenArchTypes,
  OpenArchToolBoxFrame,OpenArchDesignFrame, OpenArchNavigatorFrame,
  OpenArchObjectInfoFrame, OpenArchMainToolbar, OpenArchUIManagement,
  //GLScene:
  GLScene, GLGraph, GLObjects, GLGeomObjects, GLViewer, GLColor, GLWindows,
  GLMultiPolygon, GLCadencer,
  GLVectorGeometry;

type

  { TMainForm }

  TMainForm = class(TForm)
    MenuEdit: TMenuItem;
    MenuBuilding: TMenuItem;
    MenuArchitecture: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuMEP: TMenuItem;
    N6: TMenuItem;
    N5: TMenuItem;
    MenuStorySettings: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N2: TMenuItem;
    MenuWall: TMenuItem;
    PlanViewer: TArchPlanViewer;
    PerspectiveViewer: TArch3dViewer;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuProject: TMenuItem;
    MenuProjectSave: TMenuItem;
    MenuExit: TMenuItem;
    N1: TMenuItem;
    MenuProjectClose: TMenuItem;
    ArchStatusBar: TStatusBar;
    XY: TGLXYZGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure MenuStorySettingsClick(Sender: TObject);
    procedure MenuWallClick(Sender: TObject);
    procedure Refresh();
    function GetMessageToSendChange(aState: TArchProjectState; aElementHandled: TArchElementType;
      AContextInfo:TArchContextInfo):TArchMessage;
    procedure LoadOpenGL;
  private

  public
    //ArchProject: TArchProject;
    UIManagement: TArchUIManagement;
    // Frames:
    MainToolBar: TArchMainToolBarFrame;
    ToolBox: TArchToolBoxFrame;
    ArchDesign: TArchDesignFrame;
    Navigator: TArchNavigatorFrame;
    ObjectInfo: TArchObjectInfoFrame;
  end;

var
  MainForm: TMainForm;

implementation
uses
  //OpenGL:
  GLext, GL, glu,
  //OpenArch Parameters:
  OpenArchSysParameters,
  // OpenArch Forms:
  OpenArchProjectInfoForm, OpenArchLayersForm, OpenArchStoriesForm,
  OpenArchLineTypesForm, OpenArchFillTypesForm;

{$R *.frm}

{ TMainForm }


{ When Creating the form, a graphic interface manager objct is created, Wick will
  manage the File loading and creation of other interface elements:}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  ActiveLanguage:= 1;
  //Create UIManagement passing the Path of file:
  UIManagement:= TArchUIManagement.Create(Self,'Template/Openarch Template.oatp');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(UIManagement);
  FreeAndNil(MainToolBar);
  FreeAndNil(ToolBox);
  FreeAndNil(Navigator);
  FreeAndNil(ObjectInfo);
  FreeAndNil(ArchDesign);
end;

procedure TMainForm.MenuItem17Click(Sender: TObject);
var
  LineTypesFrm: TLineTypesForm;
begin
  LineTypesFrm:=TLineTypesForm.Create(Application,UIManagement.ArchProject.GraphicResources.LineTypes);
  LineTypesFrm.ShowModal;
end;

procedure TMainForm.MenuItem18Click(Sender: TObject);
var
  FillTypesFrm: TFillTypesForm;
begin
  FillTypesFrm:=TFillTypesForm.Create(Application);
  FillTypesFrm.ShowModal;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
var
  ProjectInforFrm: TProjectInfoForm;
begin
  ProjectInforFrm:=TProjectInfoForm.Create(Application,UIManagement.ArchProject.ProjectInfo);
  ProjectInforFrm.ShowModal;
end;

procedure TMainForm.MenuItem9Click(Sender: TObject);
var
  LayersForm: TLayersForm;
begin
  LayersForm:= TLayersForm.Create(Application,UIManagement.ArchProject.Layers, UIManagement.ArchProject.LayersSet);
  LayersForm.ShowModal;
end;

procedure TMainForm.MenuStorySettingsClick(Sender: TObject);
var
  StoriesForm: TStoriesForm;
begin
  StoriesForm:= TStoriesForm.Create(Application, UIManagement.ArchProject.Stories);
  StoriesForm.ShowModal;
end;

procedure TMainForm.MenuWallClick(Sender: TObject);
begin
  UIManagement.ArchProject.ChangeState(GetMessageToSendChange(psNewElement,etWall,ArchDesign.PlanViewer.ArchContextInfo));
end;

procedure TMainForm.Refresh();
begin
  PlanViewer.Refresh();

end;

function TMainForm.GetMessageToSendChange(aState: TArchProjectState;
  aElementHandled: TArchElementType; AContextInfo:TArchContextInfo): TArchMessage;
begin
  SetLength(Result,3);
  with Result[0] do
  begin
    ParamName:= ARCH_PROJECT_STATE;
    ParamType:= 5;
    ParamPSValue:= aState;
  end;
  with Result[1] do
  begin
    ParamName:= ARCH_ELEMENT_TYPE;
    ParamType:= 6;
    ParamETValue:= aElementHandled;
  end;
  with Result[2] do
  begin
    ParamName:= ARCH_CONTEXT_INFO;
    ParamType:= 7;
    ParamCIValue:= AContextInfo;
  end;
end;

{ The Ideaof this procedure would be verify the version of the OpenGL in user machine,
  if your version is less than 3.0 the software dont start.
  But in some tests it's don't work}
procedure TMainForm.LoadOpenGL;
begin
  //init OpenGL and load extensions
  if Load_GL_VERSION_4_0 = false then
    if Load_GL_VERSION_3_3 = false then
      if Load_GL_VERSION_3_2 = false then
        if Load_GL_VERSION_3_0 = false then
        begin
          ShowMessage('Look out!, OpenARCH work with OpenGL 3.0 or higher.');
          HALT;
        end;
  //print out OpenGL vendor, version and shader version
  ShowMessage( 'Vendor: ' + glGetString( GL_VENDOR ) );
  ShowMessage( 'OpenGL Version: ' + glGetString( GL_VERSION ) );
  ShowMessage( 'Shader Version: ' + glGetString( GL_SHADING_LANGUAGE_VERSION ) );
end;

end.

