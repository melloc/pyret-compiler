#lang pyret

provide *

import ast as A
import "ast-anf.arr" as AN

# Data definition for H constructs
# Let's have tostring() functions that print out a surface representation.
# We'll need some way of handling depth for that. We'll deal with that later. 

data Bind:
  | h-bind(id :: String, ann :: A.Ann)
end

data HLettable:
  | h-undefined with: 
    tosyn(self): "undefined" end
  | h-box(id :: String) with: 
    tosyn(self): "box " + self.id end
  | h-id(id :: String) with:
    tosyn(self): self.id end # TODO something else? 
  | h-unbox(id :: String) with: 
    tosyn(self): "unbox " + self.id end
  | h-lam(f :: String, closure :: String)
  | h-app(f :: String, args :: List<String>) with:
    tosyn(self): self.f + "(" + self.args.str-join(",") + ")" end
  | h-obj(fields :: List<HField>)
  | h-update(super :: String, fields :: List<HField>)
  | h-extend(super :: String, fields :: List<HField>) # TODO merge?
  | h-dot(obj :: String, field :: Bind)
  | h-colon(obj :: String, field :: Bind)
  | h-get-bang(obj :: String, field :: Bind) # more?
  | h-closure(fields :: List<HField>)
  | h-closure-lookup(closure :: String, field :: String)
  | h-cases # TODO
end

# We are, for now, just saving the binds as such. 
data HExpr:
  | h-ret(id :: String) with:
    tosyn(self): "ret " + self.id end
  | h-let(bind :: Bind, val :: HLettable, body :: HExpr) with: 
    tosyn(self): 
      self.bind.id + " : " + self.bind.ann + " = " + self.val.tosyn() 
        + "\n" + self.body.tosyn() 
    end
  | h-assign(bind :: Bind, val :: String, body :: HExpr) 
  | h-try(body :: HExpr, bind :: Bind, _except :: HExpr)
  | h-if(c :: String, t :: HExpr, e :: HExpr)
end

# and other types necessary for data expressions: 
data HVariant: 
  | h-variant(name :: String, 
              members :: List<N.AVariantMember>, 
              with-members :: List<HField>)
  | h-singleton-variant(name :: String, 
                        with-members :: List<HField>) 
end

data HField:
  h-field(name :: String, value :: String)
end
# The value of a field can be a string, since we lift the lookup out 
# beforehand. If that lookup is in fact just an id, we will substitute
# it back in before generating code. 


# Internal data type for representing data expressions
data NamedData:
  | named-data(name :: String, 
               variants :: List<HVariant>, 
               shared :: List<HField>,
               closure :: Set<String>)
end

data NamedFunc:
  | named-func(name :: String, 
               args :: List<HBind>, 
               body :: HExpr,
               ret :: A.Ann)
end

data NamedString:
  | named-str(name :: String, val :: String)
end


