unit PropFormatAttributeUnit;

interface

type PropFormatAttribute = class(TCustomAttribute)
  private var FAttribute: boolean;
 	public constructor Create(const attribute: boolean = true);
  public function IsAttribute(): boolean;
end;

implementation

constructor PropFormatAttribute.Create(const attribute: boolean);
begin
  self.FAttribute := attribute;
end;

function PropFormatAttribute.IsAttribute(): boolean;
begin
  result:= self.FAttribute;
end;

end.
