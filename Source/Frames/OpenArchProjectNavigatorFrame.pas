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
 2021 04 17 - ET - Unit implementation Start's.
}

unit OpenArchProjectNavigatorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Grids, Graphics,
   Types,
  //OpenArch:
  OpenArchStory, OpenArchUIManagement;

type

  { TArchProjectNavigatorFrame }

  TArchProjectNavigatorFrame = class(TFrame)
    Arch3DDocumentPage: TPage;
    Arch3DPage: TPage;
    ArchDetailPage: TPage;
    ArchElevationPage: TPage;
    ArchInteriorElevationPage: TPage;
    ArchNotebookProject: TNotebook;
    ArchSectionsPage: TPage;
    ArchSketch2DPage: TPage;
    ArchSpreadSheetsPage: TPage;
    ArchStoriesPage: TPage;
    StringGrid1: TStringGrid;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    procedure LoadStories;

  public
    UIManagement:TArchUIManagement;
    ProjectStories: TArchStories;
    constructor Create(TheOwner: TComponent; AUIManagement:TArchUIManagement); overload;
  end;

implementation
uses
  OpenArchNavigatorFrame, Dialogs;

{$R *.frm}

{ TArchProjectNavigatorFrame }

procedure TArchProjectNavigatorFrame.StringGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  vCol,vRow : integer;
begin
  StringGrid1.MouseToCell(X,Y,vCol,vRow);
  StringGrid1.Row:= vRow;
  if (y< (StringGrid1.RowCount * StringGrid1.DefaultRowHeight)) then
    StringGrid1.Cursor:= crHandPoint
  else StringGrid1.Cursor:= crDefault;
end;

procedure TArchProjectNavigatorFrame.StringGrid1DrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  i, numOfStory:integer;
  StrEnableIndex:string;
begin
  StrEnableIndex:=IntToStr(ProjectStories.StoryEnable.NumStory);
  if StringGrid1.Cells[1,aRow]=StrEnableIndex then
  begin
    StringGrid1.Canvas.Brush.Color:=clSilver;
    StringGrid1.canvas.fillRect(aRect);
    StringGrid1.canvas.TextOut(aRect.Left+2,aRect.Top+4,StringGrid1.Cells[aCol,ARow]);
  end;
end;

procedure TArchProjectNavigatorFrame.StringGrid1DblClick(Sender: TObject);
begin
  ProjectStories.SetEnable(StrToInt(StringGrid1.Cells[1,StringGrid1.Row]));
  LoadStories;
  UIManagement.RefreshStoryEnable;
end;


procedure TArchProjectNavigatorFrame.LoadStories;
var
  firstPosition, checkPosition, n, i: integer;
begin
  n:= ProjectStories.StoriesCount;
  firstPosition:=ProjectStories.GetTopStoryNum;
  StringGrid1.RowCount:=0;
  while n>0 do
  begin
    for i:=0 to ProjectStories.StoriesCount-1 do
    begin
      checkPosition:=ProjectStories.Story[i].NumStory;
      if checkPosition = firstPosition then
      begin
        with StringGrid1 do
        begin
          RowCount:= RowCount+1;
          Cells[1,(RowCount-1)]:= ProjectStories.Story[i].NumStory.ToString;
          Cells[2,(RowCount-1)]:= ProjectStories.Story[i].StoryName;
          if ProjectStories.Story[i].Enabled then
          begin
            StringGrid1.Row:=RowCount-1;
          end;
        end;
        firstPosition:=firstPosition-1;
        n:= n-1;
      end;
    end;
  end;
end;

constructor TArchProjectNavigatorFrame.Create(TheOwner: TComponent;
  AUIManagement:TArchUIManagement);
var
  navFrame: TArchNavigatorFrame;
begin
  inherited Create(TheOwner);
  UIManagement:= AUIManagement;
  navFrame:= TheOwner as TArchNavigatorFrame;
  Parent:= navFrame.NoteBookNavigator.Page[0] as TPage;
  ProjectStories:= UIManagement.ArchProject.Stories;
  LoadStories;
end;

end.

