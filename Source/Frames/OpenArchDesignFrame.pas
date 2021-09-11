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

unit OpenArchDesignFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls,
  //OpenArch:
  OpenArchViewer, OpenArchUIManagement;

type

  { TArchDesignFrame }

  TArchDesignFrame = class(TFrame)
    FArch3DPage: TPage;
    FArchDesignCoolBar: TCoolBar;
    FArchNoteBook: TNotebook;
    FArchStoriesPage: TPage;
    FArchTabControl: TTabControl;
    procedure FArchTabControlChange(Sender: TObject);
  private

  public
    UIManagement:TArchUIManagement;
    PlanViewer: TArchPlanViewer;
    PerspectiveViewer: TArch3dViewer;
    constructor Create(TheOwner: TComponent; AUIManagement:TArchUIManagement); overload;
    procedure RefreshStoryEnable;
  end;

implementation

{$R *.frm}

{ TArchDesignFrame }

procedure TArchDesignFrame.FArchTabControlChange(Sender: TObject);
begin
  FArchNoteBook.PageIndex:= FArchTabControl.TabIndex;
end;

constructor TArchDesignFrame.Create(TheOwner: TComponent; AUIManagement:TArchUIManagement);
begin
  inherited Create(TheOwner);
  Parent:= TheOwner as TForm;
  Align:=alClient;
  UIManagement:= AUIManagement;
  PlanViewer:= TArchPlanViewer.Create(FArchStoriesPage,UIManagement);
  PerspectiveViewer:= TArch3dViewer.Create(FArch3DPage,UIManagement);
  FArchTabControl.Tabs.Strings[0]:='Pavimento '+'['+UIManagement.ArchProject.Stories.StoryEnable.StoryName+']';
end;

procedure TArchDesignFrame.RefreshStoryEnable;
begin
  PlanViewer.SetCamera;
  FArchTabControl.Tabs.Strings[0]:='Pavimento '+'['+UIManagement.ArchProject.Stories.StoryEnable.StoryName+']';
end;


end.

