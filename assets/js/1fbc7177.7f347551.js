"use strict";(self.webpackChunkengineering_iog_io=self.webpackChunkengineering_iog_io||[]).push([[972],{3905:function(e,t,n){n.d(t,{Zo:function(){return c},kt:function(){return m}});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var u=r.createContext({}),s=function(e){var t=r.useContext(u),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},c=function(e){var t=s(e.components);return r.createElement(u.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},d=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,u=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),d=s(n),m=a,f=d["".concat(u,".").concat(m)]||d[m]||p[m]||i;return n?r.createElement(f,o(o({ref:t},c),{},{components:n})):r.createElement(f,o({ref:t},c))}));function m(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,o=new Array(i);o[0]=d;var l={};for(var u in t)hasOwnProperty.call(t,u)&&(l[u]=t[u]);l.originalType=e,l.mdxType="string"==typeof e?e:a,o[1]=l;for(var s=2;s<i;s++)o[s]=n[s];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}d.displayName="MDXCreateElement"},917:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return l},contentTitle:function(){return u},metadata:function(){return s},assets:function(){return c},toc:function(){return p},default:function(){return m}});var r=n(7462),a=n(3366),i=(n(7294),n(3905)),o=["components"],l={slug:"2022-02-01-haskell-nix-january-update",title:"haskell.nix January Update",authors:[],tags:["nix"],custom_edit_url:null},u="**What Happened in Haskell.Nix?**",s={permalink:"/2022-02-01-haskell-nix-january-update",editUrl:"https://github.com/input-output-hk/engineering/tree/master/blog/2022-02-01-haskell-nix-january-update-vNau7aVn4Q-import.md",source:"@site/blog/2022-02-01-haskell-nix-january-update-vNau7aVn4Q-import.md",title:"haskell.nix January Update",description:"January 2022",date:"2022-02-01T00:00:00.000Z",formattedDate:"February 1, 2022",tags:[{label:"nix",permalink:"/tags/nix"}],readingTime:1.215,truncated:!1,authors:[],frontMatter:{slug:"2022-02-01-haskell-nix-january-update",title:"haskell.nix January Update",authors:[],tags:["nix"],custom_edit_url:null},prevItem:{title:"GHC January 2022 update",permalink:"/2022-02-01-ghc-january-2022-update"}},c={authorsImageUrls:[]},p=[{value:"<strong>January 2022</strong>",id:"january-2022",children:[{value:"Changes",id:"changes",children:[],level:4},{value:"Version Updates",id:"version-updates",children:[],level:4},{value:"Bug fixes",id:"bug-fixes",children:[],level:4}],level:2}],d={toc:p};function m(e){var t=e.components,n=(0,a.Z)(e,o);return(0,i.kt)("wrapper",(0,r.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h2",{id:"january-2022"},(0,i.kt)("strong",{parentName:"h2"},"January 2022")),(0,i.kt)("p",null,"This month we merged some very significant improvements to the support for compiling for Android and iOS based AArch64 devices.\xa0 When the build system is also AArch64 template haskell can often be run locally.\xa0 This will make targeting mobile devices from AArch64 builders much easier."),(0,i.kt)("p",null,"A long running branch containing bug fixes for cross compilation to JavaScript with GHCJS was merged.\xa0 One nice feature included is better support for adding bindings to C code compiled with emscripten.\xa0 In some cases it can be as easy as adding a single JavaScript file to the package with wrappers for the C functions."),(0,i.kt)("h4",{id:"changes"},"Changes"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Much improved AArch64 support including Template Haskell (#1316)"),(0,i.kt)("li",{parentName:"ul"},"Improved GHCJS and support for calling C code compiled with emscripten (#1311)"),(0,i.kt)("li",{parentName:"ul"},"The environment variables LANG and LOCALE_ARCHIVE are no longer set in shells allowing the users prefered settings to persist (#1341)."),(0,i.kt)("li",{parentName:"ul"},"source-repo-override argument added for cabal projects to allow the location of source-repository-package packages to be replaced (#1354)")),(0,i.kt)("h4",{id:"version-updates"},"Version Updates"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"GHC 9.0.2 was added to the available GHC versions (#1338)"),(0,i.kt)("li",{parentName:"ul"},"The nixpkgs pins for 21.05, 21.11 and unstable were all updated (#1334)."),(0,i.kt)("li",{parentName:"ul"},"Remaining uses of cabal 3.4 were updated to 3.6.2 (#1328)")),(0,i.kt)("h4",{id:"bug-fixes"},"Bug fixes"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Dwarf build of ghc 9.2.1 now skipped on hydra to work around 4GB hydra limit (#1333)"),(0,i.kt)("li",{parentName:"ul"},"Removed use of propagatedBuildInputs in ghc derivation (#1318)."),(0,i.kt)("li",{parentName:"ul"},"Caching of the check-hydra CI script was fixed (#1340)")))}m.isMDXComponent=!0}}]);