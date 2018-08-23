# Brief Overview of the current implementation
## DSL : 

### DSLHelpers :

Set of helper functions that are here to garantie/help solve certain issues : 
  - Wrap properties and events to the correct virtual property/event (depending on which sort of Property we are defining the style property (Container, Control ...))

### DSL :

- OO approach to define DSL with optional parameters: Quite user friendly and Based/Inspired/Taken from EXF.
- Defined a Style object that represents a Styling object that is applicable to all objects. If some properties are not applicable to that object, the properties won't be taken in account.


## VDom :

The VDom = Virtual Dom is a simplified representation of the WPF tree.
Currently it is not really the case, we have a one to one relation, but we have not taken in account everything (decided to take in account some specific properties that I think were used in most cases).

One of the task that I want to think about in the near future is : 
- How to abstract, if it is even possible, some information that seem duplicated everywhere (should I even do that?). Maybe the abstraction could come from not thinking WPF, but providing a more different approach (like HTML?).


### VirtualProperty : 
 
 We define a hierarchical, tree-based, representation of the properties in WPF depending on which layer there are defined within the OO-based (WPF structure)[https://docs.microsoft.com/en-us/dotnet/api/system.windows.uielement?view=netframework-4.7.2].



### VirtualEvent :

As in VirtualProperty, we define a hierarchical, tree-based, representation of the events in WPF depending on which layer there are defined within the OO-based (WPF structure)[https://docs.microsoft.com/en-us/dotnet/api/system.windows.uielement?view=netframework-4.7.2].


### VDomTypes :

Define the types that allow to build a WPF VDom Tree :
- Defines Virtual Properties, events
- Defines WPF Events, to handle event handler before converting them to VEvents (by applying the dispatcher.). 
- Defines WPF Window
    

### VirtualPropertyDefaultValues :

Here, we define the default value that Uil will be applying to the properties, when those are not setted by the user. In general Close to the default values in WPF, but not 100% the case.



## VDomExt :

Here, we defined different extension modules :

- To update a real WPF Property from the value of a Virtual Property, when that one has changed in the meantime.
- To handle Event handling. By Adding or removing them, ensuring releasing of all resources ...
- To create the WPF object that is represented by a Virtual Node.


### VDom :

Here we provide the two important functions :
- treeDiff, which calculates a list of needed updates to go from the old VDom to the new VDom
- updateWindow, which update a real WPF window from a list of updates to be applied