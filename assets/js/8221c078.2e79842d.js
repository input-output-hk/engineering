"use strict";(self.webpackChunkengineering_iog_io=self.webpackChunkengineering_iog_io||[]).push([[1937],{3905:function(e,t,n){n.d(t,{Zo:function(){return p},kt:function(){return h}});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=r.createContext({}),c=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},p=function(e){var t=c(e.components);return r.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},d=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,s=e.parentName,p=l(e,["components","mdxType","originalType","parentName"]),d=c(n),h=a,f=d["".concat(s,".").concat(h)]||d[h]||u[h]||i;return n?r.createElement(f,o(o({ref:t},p),{},{components:n})):r.createElement(f,o({ref:t},p))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,o=new Array(i);o[0]=d;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l.mdxType="string"==typeof e?e:a,o[1]=l;for(var c=2;c<i;c++)o[c]=n[c];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}d.displayName="MDXCreateElement"},287:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return l},contentTitle:function(){return s},metadata:function(){return c},assets:function(){return p},toc:function(){return u},default:function(){return h}});var r=n(7462),a=n(3366),i=(n(7294),n(3905)),o=["components"],l={slug:"2022-09-23-ghcjs-heap-representation",title:"GHCJS heap representation",date:"September 23, 2022",authors:["sylvain"],tags:["ghc","javascript"]},s=void 0,c={permalink:"/2022-09-23-ghcjs-heap-representation",editUrl:"https://github.com/input-output-hk/engineering/tree/master/blog/2022-09-23-ghcjs-heap-representation.md",source:"@site/blog/2022-09-23-ghcjs-heap-representation.md",title:"GHCJS heap representation",description:"Introduction",date:"2022-09-23T00:00:00.000Z",formattedDate:"September 23, 2022",tags:[{label:"ghc",permalink:"/tags/ghc"},{label:"javascript",permalink:"/tags/javascript"}],readingTime:8.335,truncated:!0,authors:[{name:"Sylvain Henry",title:"Haskell DevX Engineer @ IOG",email:"sylvain.henry@iohk.io",key:"sylvain"}],frontMatter:{slug:"2022-09-23-ghcjs-heap-representation",title:"GHCJS heap representation",date:"September 23, 2022",authors:["sylvain"],tags:["ghc","javascript"]},prevItem:{title:"Model-Based Testing with QuickCheck",permalink:"/2022-09-28-introduce-q-d"},nextItem:{title:"GHCJS FFI system in the JS Backend",permalink:"/2022-08-18-js-backend-ffi"}},p={authorsImageUrls:[void 0]},u=[{value:"Introduction",id:"introduction",children:[],level:2},{value:"Heap objects",id:"heap-objects",children:[],level:2}],d={toc:u};function h(e){var t=e.components,n=(0,a.Z)(e,o);return(0,i.kt)("wrapper",(0,r.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h2",{id:"introduction"},"Introduction"),(0,i.kt)("p",null,"I recently gave a short presentation about heap objects representation in GHCJS and hence in the upcoming JS backend for GHC. This post is a summary of the content."),(0,i.kt)("h2",{id:"heap-objects"},"Heap objects"),(0,i.kt)("p",null,"GHC implements Haskell code evaluation by using graph reduction. As such Haskell\nprograms compiled by GHC use the heap to store nodes of the graph to be\nreduced and utility nodes participating in graph reduction. These nodes are:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"FUN: functions with their free variables as payload"),(0,i.kt)("li",{parentName:"ul"},"THUNK: suspensions with their free variables as payload"),(0,i.kt)("li",{parentName:"ul"},"PAP: partial application to a FUN. FUN closure and already applied arguments\nas payload."),(0,i.kt)("li",{parentName:"ul"},"IND: indirection to another heap object"),(0,i.kt)("li",{parentName:"ul"},"BLACKHOLE: used to overwrite a THUNK when it is being evaluated")),(0,i.kt)("p",null,"The heap is also used to store other values:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"CON: boxed values (saturated constructor applications) with field values as payload"),(0,i.kt)("li",{parentName:"ul"},"Other unlifted values: TSO, BCO, arrays, MutVar#, MVar#, TVar#, stacks, stack frames...")))}h.isMDXComponent=!0}}]);