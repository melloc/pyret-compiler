#lang pyret

provide *

import ast as A
import "ast-common.arr" as AC
import "ast-anf.arr" as AN

# Data definition for H constructs
# Let's have tostring() functions that print out a surface representation.
# We'll need some way of handling depth for that. We'll deal with that later. 

data HLettable:
  | h-undefined with: 
    tosyn(self): "undefined" end
  | h-box(id :: AC.Bind) with: 
    tosyn(self): "box " + self.id end
  | h-id(id :: AC.Bind) with:
    tosyn(self): self.id end # TODO something else? 
  | h-unbox(id :: String) with: 
    tosyn(self): "unbox " + self.id end
  | h-lam(f :: AC.Bind, closure :: AC.Bind)
  | h-app(f :: AC.Bind, args :: List<String>) with:
    tosyn(self): self.f + "(" + self.args.str-join(",") + ")" end
  | h-obj(fields :: List<HField>)
  | h-update(super :: AC.Bind, fields :: List<HField>)
  | h-extend(super :: AC.Bind, fields :: List<HField>) # TODO merge?
  | h-dot(obj :: AC.Bind, field :: AC.Bind)
  | h-colon(obj :: AC.Bind, field :: AC.Bind)
  | h-get-bang(obj :: AC.Bind, field :: AC.Bind) # more?
  | h-closure(fields :: List<HField>)
  | h-closure-lookup(closure :: String, field :: String)
end

data HCasesBranch:
  | h-cases-branch(name :: String, args :: List<AC.Bind>, body :: HExpr)
end

# We are, for now, just saving the binds as such. 
data HExpr:
  | h-ret(id :: AC.Bind) with:
    tosyn(self): "ret " + self.id end
  | h-let(bind :: AC.Bind, val :: HLettable, body :: HExpr) with: 
    tosyn(self): 
      self.bind.id + " : " + self.bind.ann + " = " + self.val.tosyn() 
        + "\n" + self.body.tosyn() 
    end
  | h-assign(bind :: AC.Bind, val :: String, body :: HExpr) 
  | h-try(body :: HExpr, bind :: AC.Bind, _except :: HExpr)
  | h-if(c :: AC.Bind, t :: HExpr, e :: HExpr)
  | h-cases(type :: A.Ann, val :: AC.Bind, branches :: List<HCasesBranch>, _else :: Option<HExpr>)
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
               args :: List<AC.Bind>, 
               body :: HExpr,
               ret :: A.Ann)
end

data NamedString:
  | named-str(name :: AC.Bind, val :: String)
end


