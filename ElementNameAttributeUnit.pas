unit ElementNameAttributeUnit;

interface

type ElementNameAttribute = class(TCustomAttribute)
  private var FName: string;
 	public constructor Create(const AName: string);
  public function GetName(): string;
end;

implementation

constructor ElementNameAttribute.Create(const AName: string);
begin
  FName:= AName;
end;


function ElementNameAttribute.GetName(): string;
begin
  Result:= FName;
end;

end.
