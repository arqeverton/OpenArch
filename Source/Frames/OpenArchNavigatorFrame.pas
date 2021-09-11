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
2021 17 04 - ET - Unit implementation Start's.
}

unit OpenArchNavigatorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls,
  //OpenArch:
  OpenArchProject, OpenArchProjectNavigatorFrame, OpenArchUIManagement;

type

  { TArchNavigatorFrame }

  TArchNavigatorFrame = class(TFrame)
    NoteBookNavigator: TNotebook;
    PageProject: TPage;
    PageView: TPage;
    PageLayout: TPage;
    ToolBar1: TToolBar;
    tbtnProject: TToolButton;
    tbtnView: TToolButton;
    tbtnLayout: TToolButton;
    procedure tbtnLayoutClick(Sender: TObject);
    procedure tbtnProjectClick(Sender: TObject);
    procedure tbtnViewClick(Sender: TObject);
  private
  public
    UIManagement:TArchUIManagement;
    ArchProject:TArchProject;
    constructor Create(TheOwner: TComponent; AUIManagement: TArchUIManagement); overload;
    procedure Refresh;
  end;

implementation

{$R *.frm}

{ TArchNavigatorFrame }

procedure TArchNavigatorFrame.tbtnProjectClick(Sender: TObject);
begin
  NoteBookNavigator.PageIndex:=0;
  tbtnLayout.Down:= False;
  tbtnView.Down:= False;
end;

procedure TArchNavigatorFrame.tbtnLayoutClick(Sender: TObject);
begin
  NoteBookNavigator.PageIndex:=2;
  tbtnProject.Down:= False;
  tbtnView.Down:= False;
end;

procedure TArchNavigatorFrame.tbtnViewClick(Sender: TObject);
begin
  NoteBookNavigator.PageIndex:=1;
  tbtnProject.Down:= False;
  tbtnLayout.Down:= False;
end;

constructor TArchNavigatorFrame.Create(TheOwner: TComponent;
  AUIManagement: TArchUIManagement);
var
  vProjectNavigator: TArchProjectNavigatorFrame;
begin
  inherited Create(TheOwner);
  Parent:= TheOwner as TForm;
  Align:=alRight;
  UIManagement:= AUIManagement;
  ArchProject:= UIManagement.ArchProject;
  vProjectNavigator:= TArchProjectNavigatorFrame.Create(Self,UIManagement);
  tbtnProject.Down:= True;
  NoteBookNavigator.PageIndex:=0;
end;

procedure TArchNavigatorFrame.Refresh;
//var
//  mForm: TMainForm;
begin
  //mForm:= Parent as TMainForm;
  //mForm.;
end;

end.

