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
 2021 02 10 - ET - Unit implementation Start's.
}
unit OpenArchGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { TArchPenType }

  TArchPenType = class
    private
      FOrder:integer;
      FStartColor: TColor;
      FOpacity: Double;
    protected
    public
      property Order:Integer read FOrder write FOrder;
  end;

  { TArchPenGradient }
  TArchPenGradient = class(TArchPenType)
    private
      FEndColor: TColor;
    protected
    public
  end;

  //---------------
  { TArchPenSolid }
  //---------------
  TArchPenSolid = class(TArchPenType)

  end;

  //--------------
  { TArchPenDash }
  //--------------
  TArchPenDash = class(TArchPenType)
    private
    protected
    public
      DashName:String;
      Pattern: Word;
      DisplayIndex: Integer;
      OffsetList: Array of Single;
  end;

  { TArchFillType }
  TArchFillType = class
    private
      FOrder:integer;
    protected
    public
      property Order:integer read FOrder write FOrder;
  end;

  { TArchFillGradient }
  TArchFillGradient = class(TArchFillType)
    private
      FGradientType:(gtLinear,gtRadial,gtRec,gtAngular);
      FXOffset,
      FYoffset,
      FAngle:Single;
      FStartColor,
      FEndColor: TColor;
      //FIOObject: IArchIOFile;
    protected

    public

  end;

  { TArchFillHatch }
  TArchFillHatch = class(TArchFillType)
    private
      FPattern: LongInt;
    protected

    public
      BackgroundColor: TColor;

  end;

implementation

end.

