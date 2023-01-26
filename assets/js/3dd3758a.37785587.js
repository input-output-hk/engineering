"use strict";(self.webpackChunkengineering_iog_io=self.webpackChunkengineering_iog_io||[]).push([[2646],{3905:function(e,t,n){n.d(t,{Zo:function(){return h},kt:function(){return d}});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var l=a.createContext({}),p=function(e){var t=a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},h=function(e){var t=p(e.components);return a.createElement(l.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},u=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,i=e.originalType,l=e.parentName,h=s(e,["components","mdxType","originalType","parentName"]),u=p(n),d=r,m=u["".concat(l,".").concat(d)]||u[d]||c[d]||i;return n?a.createElement(m,o(o({ref:t},h),{},{components:n})):a.createElement(m,o({ref:t},h))}));function d(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=n.length,o=new Array(i);o[0]=u;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s.mdxType="string"==typeof e?e:r,o[1]=s;for(var p=2;p<i;p++)o[p]=n[p];return a.createElement.apply(null,o)}return a.createElement.apply(null,n)}u.displayName="MDXCreateElement"},1018:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return s},contentTitle:function(){return l},metadata:function(){return p},assets:function(){return h},toc:function(){return c},default:function(){return d}});var a=n(7462),r=n(3366),i=(n(7294),n(3905)),o=["components"],s={slug:"2022-08-18-js-backend-ffi",title:"GHCJS FFI system in the JS Backend",date:"Aug 18, 2022",authors:["doyougnu"],tags:["ghc","javascript","ffi","explanation","knowledge_engineering"]},l="Table of Contents",p={permalink:"/2022-08-18-js-backend-ffi",source:"@site/blog/2022-08-18-ghcjs-ffi.md",title:"GHCJS FFI system in the JS Backend",description:"1.  The Design Space",date:"2022-08-18T00:00:00.000Z",formattedDate:"August 18, 2022",tags:[{label:"ghc",permalink:"/tags/ghc"},{label:"javascript",permalink:"/tags/javascript"},{label:"ffi",permalink:"/tags/ffi"},{label:"explanation",permalink:"/tags/explanation"},{label:"knowledge_engineering",permalink:"/tags/knowledge-engineering"}],readingTime:7.27,truncated:!1,authors:[{name:"Jeffrey M. Young",title:"Haskell DevX Engineer @ IOG",url:"https://iog.io/en/",key:"doyougnu"}],frontMatter:{slug:"2022-08-18-js-backend-ffi",title:"GHCJS FFI system in the JS Backend",date:"Aug 18, 2022",authors:["doyougnu"],tags:["ghc","javascript","ffi","explanation","knowledge_engineering"]},prevItem:{title:"GHCJS heap representation",permalink:"/2022-09-23-ghcjs-heap-representation"},nextItem:{title:"GHC DevX July 2022 Update",permalink:"/2022-08-01-ghc-update"}},h={authorsImageUrls:[void 0]},c=[],u={toc:c};function d(e){var t=e.components,n=(0,r.Z)(e,o);return(0,i.kt)("wrapper",(0,a.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("a",{parentName:"li",href:"#orgcf7b9df"},"The Design Space")),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("a",{parentName:"li",href:"#orgdca8008"},"GHCJS","\u2019","s FFI")),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("a",{parentName:"li",href:"#org461ca2a"},"Lightweight safety checks")),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("a",{parentName:"li",href:"#orge4568be"},"Returning multiple values")),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("a",{parentName:"li",href:"#org87ce79c"},"Changes in the FFI System for the JS Backend"))),(0,i.kt)("p",null,"Users of GHCJS enjoyed a rich\n",(0,i.kt)("a",{parentName:"p",href:"https://github.com/ghcjs/ghcjs/blob/master/doc/foreign-function-interface.md"},"FFI"),"\nsystem for foreign JavaScript imports. However, this has changed during our\nadaptation of GHCJS to GHC 9.x. This short post goes over the GHCJS FFI system,\nthe motivation for these changes and what the changes are. First, we must\nconsider the design space of an FFI system."),(0,i.kt)("a",{id:"orgcf7b9df"}),(0,i.kt)("h1",{id:"the-design-space"},"The Design Space"),(0,i.kt)("p",null,"FFI code is typically employed in high performance scenarios. Additionally,\nusers of the FFI ",(0,i.kt)("em",{parentName:"p"},"do not")," want to deal with the object language the compiler is\ncompiling to. Instead, users want a simple way to call functions from the object\nlanguage and use them in their own code as normal Haskell functions. However,\nusers of the FFI system ",(0,i.kt)("em",{parentName:"p"},"do")," tend to be power users, and so as a design principle\nwe want to expose the tools they need to achieve their performance needs,\nwhatever those needs may be. We can summarize these constraints as follows:"),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},"The FFI must abstract the JavaScript backend","\u2019","s infidelities away as much as\npossible. That is, users of the FFI ",(0,i.kt)("em",{parentName:"li"},"should")," need to worry about the ",(0,i.kt)("inlineCode",{parentName:"li"},"Int64#"),"\nrepresentation, but should also be able to simply follow standard patterns we\nhave written in ",(0,i.kt)("inlineCode",{parentName:"li"},"base"),"."),(0,i.kt)("li",{parentName:"ol"},"The FFI must provide tools to achieve high performance code, even if those\ntools require up front knowledge of the runtime system to use. However, these\ntools should not be in the path of least resistance to use the FFI system."),(0,i.kt)("li",{parentName:"ol"},"The FFI must provide a lightweight specification that user","\u2019","s program against\nfor the JS backend to optimize the imported function and for good error\nmessages for users.")),(0,i.kt)("p",null,"GHCJS","\u2019","s FFI sets a high (qualitative) benchmark on these three constraints.\nLet","\u2019","s inspect them each in detail, in no particular order."),(0,i.kt)("a",{id:"orgdca8008"}),(0,i.kt)("h1",{id:"ghcjss-ffi"},"GHCJS","\u2019","s FFI"),(0,i.kt)("p",null,"In GHCJS, a user could take advantage of JavaScript functions in their Haskell\ncode using the GHCJS","\u2019","s FFI. However, the syntax was unique to GHCJS with place\nholder variables like one might see in perl, nix, or bash. For example, here is\na foreign import from the ",(0,i.kt)("inlineCode",{parentName:"p"},"base")," library for ",(0,i.kt)("inlineCode",{parentName:"p"},"st_size"),":"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'-- base/System/Posix/Internal.hs\n-- the JS FFI version\nforeign import javascript unsafe "$r1 = h$base_st_size($1_1,$1_2); $r2 = h$ret1;"\n   st_size :: Ptr CStat -> IO Int64\n')),(0,i.kt)("p",null,"The syntax is different from what we know and love in the normal Haskell world\nbut the grammar is straightforward. We declare a ",(0,i.kt)("inlineCode",{parentName:"p"},"foreign import")," from ",(0,i.kt)("inlineCode",{parentName:"p"},"javascript"),",\nstate that the import is ",(0,i.kt)("inlineCode",{parentName:"p"},"unsafe")," or ",(0,i.kt)("inlineCode",{parentName:"p"},"interruptible")," and then provide a string,\n",(0,i.kt)("inlineCode",{parentName:"p"},"h$base_fstat(...)")," for the code generator to use when compiling. Compare this\nwith the C version:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'-- base/System/Posix/Internal.hs\n-- the C FFI version\nforeign import ccall unsafe "HsBase.h __hscore_st_size"\n   st_size :: Ptr CStat -> IO Int64\n')),(0,i.kt)("p",null,"And we see that they are similar. The only difference is the strange ",(0,i.kt)("inlineCode",{parentName:"p"},"$n"),"\nsymbols in the referrent string. Contrast this with the C version, which simply\ndeclares a name."),(0,i.kt)("p",null,"These symbols are ",(0,i.kt)("em",{parentName:"p"},"place holder")," variables with special meaning in GHCJS. There\nare two intractable reasons for the placeholder patterns. First, we require\nthese patterns to work around the limitations of JavaScript as a backend (1).\nFor example, consider the case where we need to return an ",(0,i.kt)("inlineCode",{parentName:"p"},"Int64#")," from an\nimported foreign function. In C and Haskell this is not a problem because both\ncan represent ",(0,i.kt)("inlineCode",{parentName:"p"},"Int64#")," natively, however JavaScript only has native support for\n32-bit values. Thus, to be able to return an ",(0,i.kt)("inlineCode",{parentName:"p"},"Int64#")," we need to have a method to\nreturn two 32-bit numbers. Similarly, in order to apply a function to an ",(0,i.kt)("inlineCode",{parentName:"p"},"Int64#"),"\nthat function must take at least two arguments, one for the high bits and one\nfor the low. Second, the referrent string is untyped and can contain arbritrary\nJavaScript code. So placeholder patterns provide a simply and lightweight way\nfor safety checks and eliminate classes of untyped, hard to understand errors.\nFor example, consider an arity mismatch error between a function definition and\ncall site. When this happens JavaScript happily continues processing with the\nreturn value from the function application defined as ",(0,i.kt)("inlineCode",{parentName:"p"},"NaN")," (of course). Such\narity conflicts can easily occur, especially when dealing with 64-bit values\nwhich require function arity assumptions."),(0,i.kt)("a",{id:"org461ca2a"}),(0,i.kt)("h1",{id:"lightweight-safety-checks"},"Lightweight safety checks"),(0,i.kt)("p",null,"Lightweight safety checks (3) are done by GHCJS by parsing the names of the\nplace holder variables; each of which follows a specific naming convention. This\nconvention is:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Argument types:",(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"$n"),": Used for unary arguments, i.e., arguments which require only a single register."),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"$n_n"),": Used for binary arguments, i.e., arguments which require two registers."),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"$c"),": A continuation argument, only valid for ",(0,i.kt)("inlineCode",{parentName:"li"},"interruptible")," foreign functions."))),(0,i.kt)("li",{parentName:"ul"},"Return types:",(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"$r"),": a unary return"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"$r1"),", ",(0,i.kt)("inlineCode",{parentName:"li"},"$r2"),": a binary return"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"$r1"),", ",(0,i.kt)("inlineCode",{parentName:"li"},"$r2"),", ",(0,i.kt)("inlineCode",{parentName:"li"},"$r3_1"),", ",(0,i.kt)("inlineCode",{parentName:"li"},"$r3_2"),": unboxed tuple return"))),(0,i.kt)("li",{parentName:"ul"},"Top level patterns:",(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},'"&value"'),": simply emitted as ",(0,i.kt)("inlineCode",{parentName:"li"},"value")," by the code generator"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},'"someFunction"'),": emitted as ",(0,i.kt)("inlineCode",{parentName:"li"},"ret = someFunction(...)"),", i.e., map the FFI to\nthe result of the function call."),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},'"$r = $1.f($2)"'),": emitted as ",(0,i.kt)("inlineCode",{parentName:"li"},"r1 = a1.f(a2)"),", i.e., a combination of a\nfunction call and a property access.")))),(0,i.kt)("p",null,"With this standard GHCJS then parses the FFI referrent string to ensure that it\nconforms to this standard. If not then GHCJS can at least respond to the user\nwith an ill-formatted FFI message ",(0,i.kt)("em",{parentName:"p"},"and")," say precisely where the issue is. For\nexample, it could respond that only half of an ",(0,i.kt)("inlineCode",{parentName:"p"},"Int64#")," is returned based on the\nreferrent string and the function type."),(0,i.kt)("a",{id:"orge4568be"}),(0,i.kt)("h1",{id:"returning-multiple-values"},"Returning multiple values"),(0,i.kt)("p",null,"But what of performant code? GHCJS achieves performant FFI by not trying to\nabstract away from the runtime system. Instead, an advantage of GHCJS","\u2019","s FFI ",(0,i.kt)("em",{parentName:"p"},"is"),"\nthat we can specify exactly which registers the foreign function should dump its\nresults or even arbitrary global variables. This places more burden on the user\nof the FFI in specific scenarios, but crucially allows the FFI system to get out\nof the way of the user. The FFI system also exploits this capability to return\nmultiple values from a single function call, which is a common need when\ncompiling to JavaScript. For example, in the above code ",(0,i.kt)("inlineCode",{parentName:"p"},"st_size")," is declared to\nreturn an ",(0,i.kt)("inlineCode",{parentName:"p"},"IO Int64"),", the JavaScript handler ",(0,i.kt)("inlineCode",{parentName:"p"},"h$base_st_size")," returns the ",(0,i.kt)("inlineCode",{parentName:"p"},"Int64"),"\nusing two registers ",(0,i.kt)("inlineCode",{parentName:"p"},"$r1")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"$r2"),", but does so through the use of a special\npurpose global variable called ",(0,i.kt)("inlineCode",{parentName:"p"},"h$ret1"),":"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},"function h$base_st_size(stat, stat_off) {\n    h$ret1 = (stat.i3[(stat_off>>2)+2]);\n    return (stat.i3[(stat_off>>2)+1]);\n}\n")),(0,i.kt)("p",null,"The function inputs a pointer and an offset. Pointers in GHCJS are simply\npointers to ByteArrays so the function indexes into the ByteArray and retrieves\nand stores the lower 32-bits in ",(0,i.kt)("inlineCode",{parentName:"p"},"h$ret1"),", then returns the higher 32-bits\ndirectly. These results are picked up by the FFI code, which performs assignment\nto set ",(0,i.kt)("inlineCode",{parentName:"p"},"$r1")," to the result of the function call (the higher 32-bits), and set ",(0,i.kt)("inlineCode",{parentName:"p"},"$r2"),"\nto the value of ",(0,i.kt)("inlineCode",{parentName:"p"},"h$ret1")," (the lower 32-bits). Crucially, the runtime system needs\nto do nothing. The registers are already handled ready to be consumed by\nwhatever the caller of the foreign function will do."),(0,i.kt)("p",null,"One might consider using a simpler design, which trades register juggling for a\nmore straightforward representation such as a ByteArray which stores the ",(0,i.kt)("inlineCode",{parentName:"p"},"Int64#"),".\nHowever, such a design would trade speed for implementation simplicity. If we\npassed ByteArrays then each foreign function would spend time wrapping and\nunwrapping the array to get the payload; clearly an undesirable outcome for high\nperformance code."),(0,i.kt)("a",{id:"org87ce79c"}),(0,i.kt)("h1",{id:"changes-in-the-ffi-system-for-the-js-backend"},"Changes in the FFI System for the JS Backend"),(0,i.kt)("p",null,"So we see that GHCJS","\u2019","s FFI system actually performs quite well in the design\nspace. Power users are well supported and can leverage enough unsafety to bind\nglobal variables like ",(0,i.kt)("inlineCode",{parentName:"p"},"h$ret1")," and specific registers such as ",(0,i.kt)("inlineCode",{parentName:"p"},"$r1"),". The system\nprovides some lightweight checking through parsing. The nuances of the\nJavaScript platform are generally abstracted over and the FFI system is tuned\nfor performance critical scenarios. So why change it?"),(0,i.kt)("p",null,"The short answer is to hit deadlines. By skipping the FFI parsing the JS Backend\nteam was able to produce a working (can output ","\u201c","Hello World!","\u201d",", and compile GHC","\u2019","s\nboot libraries), integrated, JS backend in GHC faster than had we finished the\nFFI system."),(0,i.kt)("p",null,"For the time being, we have opted to replaced each foreign function call with a\nJavaScript fat arrow, for example:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre"},'foreign import javascript unsafe "(($1_1,$1_2) => { return h$base_st_size($1_1,$1_2); })"\n   st_size :: Ptr CStat -> IO Int64\n')),(0,i.kt)("p",null," Of course, this situation is untenable, as argued above, FFI code is assumed to\nbe used in performance critical code, and thus any extra overhead, such as a\nfunction closure and consequent indirection, must be avoided. But fear not! In\nthe near future we","\u2019","ll be overhauling the FFI system and returning it to its\nformer glory."))}d.isMDXComponent=!0}}]);