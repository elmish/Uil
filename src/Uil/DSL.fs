namespace Uil

module DSL =    


    module DSLHelpers =
        open VDom.VDomTypes
        open VDom.VirtualProperty

        let (|IsContainer|_|) (constr: 'a -> VProperty) (tag:Tag<'Msg>) =
            match tag with 
            | NodeContainer _ -> Some constr
            | _           -> None

        let (|IsControl|_|) (constr: 'a -> VProperty) (tag:Tag<'Msg>) =
            match tag with 
            | NodeSingle (TaggedControl _) -> Some constr
            | _         -> None

        let (|IsItems|_|) (constr: 'a -> VProperty) (tag:Tag<'Msg>) =
            match tag with 
            | NodeItems _ -> Some constr
            | _         -> None

        let (|IsTextBlock|_|) (constr: 'a -> VProperty) (tag:Tag<'Msg>) =
            match tag with 
            | NodeSingle (TaggedTextBlock _) -> Some constr
            | _         -> None

        let (|IsBorder|_|) (constr: 'a -> VProperty) (tag:Tag<'Msg>) =
            match tag with 
            | NodeSingle (TaggedBorder _) -> Some constr
            | _         -> None



        let (>||) ((|AP1|_|): Tag<'Msg> -> ('a -> VProperty) option) ((|AP2|_|): Tag<'Msg> -> ('a -> VProperty) option) =
            fun (tag:Tag<'Msg>) ->
                match (|AP1|_|) tag with
                | Some constr -> Some constr
                | None ->
                    match (|AP2|_|) tag with
                    | Some constr -> Some constr
                    | None        -> None                    


        let internal bindVProperties (x:'a option) (constr: 'a -> VProperty) (listAndIndex:((VProperty*int) list * int)) =
            let (list,index) = listAndIndex
            let index = index + 1
            match x with
            | None -> (list,index)
            | Some a -> ((constr a,index)::list,index)

        let internal bindVPropertiesWithMatch (x:'a option) (AP:(Tag<'Msg> -> ('a -> VProperty) option)) (tag:Tag<'Msg>) (listAndIndex:((VProperty*int) list * int)) =
            let (list,index) = listAndIndex
            let index = index + 1
            match x with
            | None -> (list,index)
            | Some a -> 
                match AP tag with
                | Some constr -> ((constr a,index)::list,index)
                | None        -> (list,index)

        let internal  bindWPFEvents (x:option<'a -> 'Msg>) (builder: (obj -> 'a -> unit) -> 'b) (constr: WPFLambda<'Msg,'a,'b> -> WPFEvent<'Msg>) (listAndIndex:((WPFEvent<'Msg>*int) list * int)) =
            let (list,index) = listAndIndex
            let index = index + 1
            match x with
            | None -> (list,index)
            | Some a -> ((constr (a,builder),index)::list,index)


    module DSL =
        open System
        open System.Windows
        open System.Windows.Media
        open System.Windows.Controls
        open System.ComponentModel
        open VDom.VDomTypes
        open VDom
        open VirtualProperty
        open DSLHelpers
        open System.Windows.Markup
        open VDom.VirtualProperty.FsWPFRepresentation
        open System.Windows.Controls.Primitives



        type Style( //1.  
                     ?Column               
                    ,?ColumnSpan           
                    ,?Row                  
                    ,?RowSpan     
                    ,?Bottom
                    ,?Left
                    ,?Right
                    ,?Top
                    ,?IsEnabled
                    ,?Opacity
                    ,?Visibility
                    //2. 
                    ,?Height
                    ,?HorizontalAlignment
                    ,?Margin
                    ,?VerticalAlignment
                    ,?Width
                    //3. 
                    ,?BorderBrush
                    ,?BorderThickness
                    ,?FontFamily
                    ,?FontSize
                    ,?FontStretch
                    ,?FontStyle
                    ,?FontWeight
                    ,?Foreground
                    ,?HorizontalContentAlignment
                    ,?Padding
                    ,?VerticalContentAlignment
                    ,?Background 
                    
                    ) =

            member private __.BindedVProperties (list, tag : Tag<'Msg>) =
                list
                //1.  
                |> bindVProperties Column       (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.Column       x)))
                |> bindVProperties ColumnSpan   (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.ColumnSpan   x)))        
                |> bindVProperties Row          (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.Row          x)))        
                |> bindVProperties RowSpan      (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.RowSpan      x)))        
                |> bindVProperties Bottom       (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.Bottom       x)))
                |> bindVProperties Left         (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.Left         x)))
                |> bindVProperties Right        (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.Right        x)))
                |> bindVProperties Top          (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.Top          x)))
                |> bindVProperties IsEnabled    (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.IsEnabled    x)))
                |> bindVProperties Opacity      (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.Opacity      x)))
                |> bindVProperties Visibility   (fun x -> VProperty.VStyle (UIElementStyle (UIElementStyle.Visibility   x)))
                //2. 
                |> bindVProperties Height               (fun x -> VProperty.VStyle (FrameworkElementStyle (FrameworkElementStyle.Height                 x)))
                |> bindVProperties HorizontalAlignment  (fun x -> VProperty.VStyle (FrameworkElementStyle (FrameworkElementStyle.HorizontalAlignment    x)))
                |> bindVProperties Margin               (fun x -> VProperty.VStyle (FrameworkElementStyle (FrameworkElementStyle.Margin                 x)))
                |> bindVProperties VerticalAlignment    (fun x -> VProperty.VStyle (FrameworkElementStyle (FrameworkElementStyle.VerticalAlignment      x)))
                |> bindVProperties Width                (fun x -> VProperty.VStyle (FrameworkElementStyle (FrameworkElementStyle.Width                  x)))
                //3.
                |> bindVPropertiesWithMatch HorizontalContentAlignment   ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.HorizontalContentAlignment   x)))) tag
                |> bindVPropertiesWithMatch VerticalContentAlignment     ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.VerticalContentAlignment     x)))) tag
                //3.
                |> bindVPropertiesWithMatch 
                        Background                   
                        (     ((|IsContainer|_|) (fun x -> VProperty.VStyle (PanelStyle (PanelStyle.Background          x))))
                          >|| ((|IsControl|_|)   (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.Background      x))))
                          >|| ((|IsBorder|_|)    (fun x -> VProperty.VStyle (DecoratorStyle (BorderStyle (BorderStyle.Background      x)))))
                          >|| ((|IsTextBlock|_|) (fun x -> VProperty.VStyle (TextBlockStyle (TextBlockStyle.Background  x))))  ) 
                        tag
                |> bindVPropertiesWithMatch
                        BorderBrush                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.BorderBrush                  x)))) 
                          >||  ((|IsBorder|_|) (fun x -> VProperty.VStyle (DecoratorStyle (BorderStyle (BorderStyle.BorderBrush                  x)))))  )
                        tag
                |> bindVPropertiesWithMatch
                        BorderThickness                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.BorderThickness                  x)))) 
                          >||  ((|IsBorder|_|) (fun x -> VProperty.VStyle (DecoratorStyle (BorderStyle (BorderStyle.BorderThickness                  x)))))  )
                        tag
                |> bindVPropertiesWithMatch 
                        FontFamily                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.FontFamily       x)))) 
                          >||  ((|IsTextBlock|_|) (fun x -> VProperty.VStyle (TextBlockStyle (TextBlockStyle.FontFamily x))))  )
                        tag
                |> bindVPropertiesWithMatch 
                        FontSize                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.FontSize         x)))) 
                          >||  ((|IsTextBlock|_|) (fun x -> VProperty.VStyle (TextBlockStyle (TextBlockStyle.FontSize   x))))  )
                        tag                    
                |> bindVPropertiesWithMatch 
                        FontStretch                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.FontStretch         x)))) 
                          >||  ((|IsTextBlock|_|) (fun x -> VProperty.VStyle (TextBlockStyle (TextBlockStyle.FontStretch   x))))  )
                        tag                    
                |> bindVPropertiesWithMatch 
                        FontStyle                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.FontStyle         x)))) 
                          >||  ((|IsTextBlock|_|) (fun x -> VProperty.VStyle (TextBlockStyle (TextBlockStyle.FontStyle   x))))  )
                        tag                    
                |> bindVPropertiesWithMatch 
                        FontWeight                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.FontWeight         x)))) 
                          >||  ((|IsTextBlock|_|) (fun x -> VProperty.VStyle (TextBlockStyle (TextBlockStyle.FontWeight   x))))  )
                        tag                    
                |> bindVPropertiesWithMatch 
                        Foreground                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.Foreground         x)))) 
                          >||  ((|IsTextBlock|_|) (fun x -> VProperty.VStyle (TextBlockStyle (TextBlockStyle.Foreground   x))))  )
                        tag                    
                |> bindVPropertiesWithMatch 
                        Padding                   
                        (      ((|IsControl|_|) (fun x -> VProperty.VStyle (ControlStyle (ControlStyle.Padding         x)))) 
                          >||  ((|IsTextBlock|_|) (fun x -> VProperty.VStyle (TextBlockStyle (TextBlockStyle.Padding   x))))  )
                        tag                    
        
            static member internal PropagateStyle (style:Style option) tag list =
                match style with
                | Some style -> style.BindedVProperties(list,tag)
                | None       -> list





        type WPF =    

            (*** ****************** ***) 
            (***      Button        ***) 
            (*** ****************** ***) 
            static member button( ?Content              : obj  
                                 ,?Click                : RoutedEventArgs -> 'Msg 
                                 ,?style                : Style ) = 
                           
                let tag = Tag.NodeSingle (TaggedControl Button)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties Content              (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty  (SingleContentProperty.Content  x))))
                    |> Style.PropagateStyle style tag 
                    |> fst
                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents Click (fun x -> new RoutedEventHandler(x)) WPFClick                    
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = vevents |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node                



            (*** ****************** ***) 
            (***      TextBlock     ***) 
            (*** ****************** ***) 
            static member textBlock( ?Text                  : string 
                                    ,?TextAlignment         : TextAlignment 
                                    ,?TextWrapping          : TextWrapping 
                                    ,?TextChanged           : TextChangedEventArgs -> 'Msg 
                                    ,?style                 : Style ) =                   
            
                let tag = Tag.NodeSingle (TaggedTextBlock TextBlock)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties  TextAlignment         (fun x -> VProperty.FEProperty (TextBlockProperty (TextBlockProperty.TextAlignment x)))
                    |> bindVProperties  TextWrapping          (fun x -> VProperty.FEProperty (TextBlockProperty (TextBlockProperty.TextWrapping x)))    
                    |> bindVProperties  Text                  (fun x -> VProperty.FEProperty (TextBlockProperty (TextBlockProperty.Text x)))     
                    |> Style.PropagateStyle style tag 
                    |> fst

                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents TextChanged (fun x -> new TextChangedEventHandler(x)) WPFTextChanged
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = vevents |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node  


            (*** ****************** ***) 
            (***      CheckBox      ***) 
            (*** ****************** ***) 
            static member checkBox( ?Content              : obj  
                                   ,?IsChecked            : bool
                                   ,?IsThreeState         : bool
                                   ,?Click                : RoutedEventArgs -> 'Msg 
                                   ,?Checked              : RoutedEventArgs -> 'Msg 
                                   ,?Unchecked            : RoutedEventArgs -> 'Msg 
                                   ,?Indeterminate        : RoutedEventArgs -> 'Msg  
                                   ,?style                : Style   ) = 
                           
                let tag = Tag.NodeSingle (TaggedControl CheckBox)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties Content              (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (SingleContentProperty.Content  x))))   
                    |> bindVProperties IsChecked            (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (ButtonBaseProperty (ToggleButtonProperty (ToggleButtonProperty.IsChecked x))))))
                    |> bindVProperties IsThreeState         (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (ButtonBaseProperty (ToggleButtonProperty (ToggleButtonProperty.IsThreeState x))))))       
                    |> Style.PropagateStyle style tag 
                    |> fst
                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents Click (fun x -> new RoutedEventHandler(x)) WPFClick                    
                    |> bindWPFEvents Checked (fun x -> new RoutedEventHandler(x)) WPFChecked                    
                    |> bindWPFEvents Unchecked (fun x -> new RoutedEventHandler(x)) WPFUnchecked                    
                    |> bindWPFEvents Indeterminate (fun x -> new RoutedEventHandler(x)) WPFIndeterminate                    
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = vevents |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node               

            (*** ****************** ***) 
            (***    ToggleButton    ***) 
            (*** ****************** ***) 
            static member toggleButton( ?Content              : obj  
                                       ,?IsChecked            : bool
                                       ,?IsThreeState         : bool
                                       ,?Click                : RoutedEventArgs -> 'Msg 
                                       ,?Checked              : RoutedEventArgs -> 'Msg 
                                       ,?Unchecked            : RoutedEventArgs -> 'Msg 
                                       ,?Indeterminate        : RoutedEventArgs -> 'Msg  
                                       ,?style                : Style   ) = 
                           
                let tag = Tag.NodeSingle (TaggedControl TaggedControl.ToggleButton)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties Content              (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (SingleContentProperty.Content  x))))   
                    |> bindVProperties IsChecked            (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (ButtonBaseProperty (ToggleButtonProperty (ToggleButtonProperty.IsChecked x))))))
                    |> bindVProperties IsThreeState         (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (ButtonBaseProperty (ToggleButtonProperty (ToggleButtonProperty.IsThreeState x))))))       
                    |> Style.PropagateStyle style tag 
                    |> fst
                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents Click (fun x -> new RoutedEventHandler(x)) WPFClick                    
                    |> bindWPFEvents Checked (fun x -> new RoutedEventHandler(x)) WPFChecked                    
                    |> bindWPFEvents Unchecked (fun x -> new RoutedEventHandler(x)) WPFUnchecked                    
                    |> bindWPFEvents Indeterminate (fun x -> new RoutedEventHandler(x)) WPFIndeterminate                    
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = vevents |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node               


            (*** ****************** ***) 
            (***      RadioButton   ***) 
            (*** ****************** ***) 
            static member radioButton(  ?Content              : obj  
                                       ,?IsChecked            : bool
                                       ,?IsThreeState         : bool
                                       ,?Click                : RoutedEventArgs -> 'Msg 
                                       ,?Checked              : RoutedEventArgs -> 'Msg 
                                       ,?Unchecked            : RoutedEventArgs -> 'Msg 
                                       ,?Indeterminate        : RoutedEventArgs -> 'Msg  
                                       ,?style                : Style   ) = 
                           
                let tag = Tag.NodeSingle (TaggedControl RadioButton)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties Content              (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (SingleContentProperty.Content  x))))   
                    |> bindVProperties IsChecked            (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (ButtonBaseProperty (ToggleButtonProperty (ToggleButtonProperty.IsChecked x))))))
                    |> bindVProperties IsThreeState         (fun x -> VProperty.FEProperty (ControlProperty (SingleContentProperty (ButtonBaseProperty (ToggleButtonProperty (ToggleButtonProperty.IsThreeState x))))))       
                    |> Style.PropagateStyle style tag 
                    |> fst
                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents Click (fun x -> new RoutedEventHandler(x)) WPFClick                    
                    |> bindWPFEvents Checked (fun x -> new RoutedEventHandler(x)) WPFChecked                    
                    |> bindWPFEvents Unchecked (fun x -> new RoutedEventHandler(x)) WPFUnchecked                    
                    |> bindWPFEvents Indeterminate (fun x -> new RoutedEventHandler(x)) WPFIndeterminate                    
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = vevents |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node          


            (*** ****************** ***) 
            (***    ProgressBar     ***) 
            (*** ****************** ***) 
            static member progressBar( ?IsIndeterminate     : bool
                                      ,?Value               : float
                                      ,?Minimum             : float
                                      ,?Maximum             : float
                                      ,?Orientation         : Orientation
                                      ,?ValueChanged        : RoutedPropertyChangedEventArgs<double> -> 'Msg
                                      ,?style               : Style ) = 
                           
                let tag = Tag.NodeSingle (TaggedControl ProgressBar)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties IsIndeterminate      (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (ProgressProperty (ProgressProperty.IsIndeterminate x)))))
                    |> bindVProperties Value                (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (RangeProperty.Value x))))
                    |> bindVProperties Minimum              (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (RangeProperty.Minimum x))))                        
                    |> bindVProperties Maximum              (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (RangeProperty.Maximum x))))
                    |> bindVProperties Orientation          (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (ProgressProperty (ProgressProperty.Orientation x)))))
                    |> Style.PropagateStyle style tag 
                    |> fst
                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents ValueChanged (fun x -> new RoutedPropertyChangedEventHandler<double>(x)) WPFValueChanged
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = vevents |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node             

            (*** ****************** ***) 
            (***    Slider          ***) 
            (*** ****************** ***) 
            static member slider( ?IsIndeterminate      : bool
                                 ,?AutoToolTipPlacement : AutoToolTipPlacement
                                 ,?AutoToolTipPrecision : int
                                 ,?TickFrequency        : float 
                                 ,?TickPlacement        : TickPlacement
                                 ,?Ticks                : DoubleCollection
                                 ,?Value                : float
                                 ,?Minimum              : float
                                 ,?Maximum              : float 
                                 ,?ValueChanged         : RoutedPropertyChangedEventArgs<double> -> 'Msg
                                 ,?style                : Style ) = 
            
                let tag = Tag.NodeSingle (TaggedControl ProgressBar)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties IsIndeterminate      (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (ProgressProperty (ProgressProperty.IsIndeterminate x)))))
                    |> bindVProperties AutoToolTipPlacement (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (SliderProperty (SliderProperty.AutoToolTipPlacement x)))))
                    |> bindVProperties AutoToolTipPrecision (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (SliderProperty (SliderProperty.AutoToolTipPrecision x)))))
                    |> bindVProperties TickFrequency        (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (SliderProperty (SliderProperty.TickFrequency x)))))
                    |> bindVProperties TickPlacement        (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (SliderProperty (SliderProperty.TickPlacement x)))))
                    |> bindVProperties Ticks                (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (SliderProperty (SliderProperty.Ticks x)))))
                    |> bindVProperties Value                (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (RangeProperty.Value x))))
                    |> bindVProperties Minimum              (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (RangeProperty.Minimum x))))                        
                    |> bindVProperties Maximum              (fun x -> VProperty.FEProperty (ControlProperty (RangeProperty (RangeProperty.Maximum x))))
                    |> Style.PropagateStyle style tag 
                    |> fst

                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents ValueChanged (fun x -> new RoutedPropertyChangedEventHandler<double>(x)) WPFValueChanged
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = vevents |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node             

            (*** ****************** ***) 
            (***    ComboBox        ***) 
            (*** ****************** ***) 
            static member comboBox( children                    : WPFTree<'Msg> list
                                   ,?IsEditable                 : bool
                                   ,?Text                       : string
                                   ,?SelectedItem               : obj
                                   ,?IsTextSearchEnabled        : bool
                                   ,?IsTextSearchCaseSensitive  : bool
                                   ,?SelectionChanged           : SelectionChangedEventArgs -> 'Msg
                                   ,?style                      : Style ) = 


                let tag = Tag.NodeItems (ComboBox,children)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties IsEditable                   (fun x -> VProperty.FEProperty (ControlProperty (CollectionContentProperty (SelectableProperty (ComboBoxProperty (ComboBoxProperty.IsEditable x))))))
                    |> bindVProperties Text                         (fun x -> VProperty.FEProperty (ControlProperty (CollectionContentProperty (SelectableProperty (ComboBoxProperty (ComboBoxProperty.Text x))))))
                    |> bindVProperties SelectedItem                 (fun x -> VProperty.FEProperty (ControlProperty (CollectionContentProperty (SelectableProperty (SelectableProperty.SelectedItem x)))))
                    |> bindVProperties IsTextSearchEnabled          (fun x -> VProperty.FEProperty (ControlProperty (CollectionContentProperty (CollectionContentProperty.IsTextSearchEnabled x))))
                    |> bindVProperties IsTextSearchCaseSensitive    (fun x -> VProperty.FEProperty (ControlProperty (CollectionContentProperty (CollectionContentProperty.IsTextSearchCaseSensitive x))))
                    |> Style.PropagateStyle style tag 
                    |> fst

                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents SelectionChanged (fun x -> new SelectionChangedEventHandler(x)) WPFSelectionChanged
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = vevents |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node       

            (*** ****************** ***) 
            (***    TextBox         ***) 
            (*** ****************** ***) 
            static member textBox( ?CanEnter                    : bool
                                  ,?CanTab                      : bool
                                  ,?Language                    : XmlLanguage
                                  ,?SpellCheck                  : bool
                                  ,?Text                        : string
                                  ,?TextAlignment               : TextAlignment
                                  ,?TextDecorations             : TextDecorationCollection
                                  ,?TextWrapping                : TextWrapping
                                  ,?VerticalScrollBarVisibility : ScrollBarVisibility            
                                  ,?style                       : Style   ) = 

                let tag = Tag.NodeSingle (TaggedControl TextBox)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties CanEnter                     (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.CanEnter                     x))))
                    |> bindVProperties CanTab                       (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.CanTab                       x))))
                    |> bindVProperties Language                     (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.Language                     x))))
                    |> bindVProperties SpellCheck                   (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.SpellCheck                   x))))
                    |> bindVProperties Text                         (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.Text                         x))))
                    |> bindVProperties TextAlignment                (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.TextAlignment                x))))
                    |> bindVProperties TextDecorations              (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.TextDecorations              x))))
                    |> bindVProperties TextWrapping                 (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.TextWrapping                 x))))
                    |> bindVProperties VerticalScrollBarVisibility  (fun x -> VProperty.FEProperty (ControlProperty (TextBoxProperty (TextBoxProperty.VerticalScrollBarVisibility  x))))
                    |> Style.PropagateStyle style tag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = tag
                      Properties = vprops  |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node



            (*** ****************** ***) 
            (***      Grid          ***) 
            (*** ****************** ***) 
            static member grid( ?ColumnDefinitions  : ColDef list 
                               ,?RowDefinitions     : RowDef list 
                               ,?style              : Style
                               ,?Children           : WPFTree<'Msg> list  ) =
                                    
                let fakeTag = Tag.NodeContainer(Grid, [])
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties  ColumnDefinitions     (fun x -> VProperty.FEProperty (PanelProperty (GridProperty (GridProperty.ColumnDefinitions x))))
                    |> bindVProperties  RowDefinitions        (fun x -> VProperty.FEProperty (PanelProperty (GridProperty (GridProperty.RowDefinitions x))))
                    |> Style.PropagateStyle style fakeTag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = Tag.NodeContainer(Grid, defaultArg Children [])
                      Properties = vprops   |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node


            (*** ****************** ***) 
            (***      StackPanel    ***) 
            (*** ****************** ***) 
            static member stackPanel( ?style            : Style
                                     ,?Orientation      : Orientation
                                     ,?Children         : WPFTree<'Msg> list ) =
                                     
                let fakeTag = Tag.NodeContainer(StackPanel,[])
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties  Orientation     (fun x -> VProperty.FEProperty (PanelProperty (StackPanelProperty (StackPanelProperty.Orientation x))))
                    |> Style.PropagateStyle style fakeTag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = Tag.NodeContainer(StackPanel,defaultArg Children [])
                      Properties = vprops   |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node



            (*** ****************** ***) 
            (***      Canvas        ***) 
            (*** ****************** ***) 
            static member canvas( ?style    : Style 
                                 ,?Children : WPFTree<'Msg> list  ) =
                                     
                let fakeTag = Tag.NodeContainer(Canvas,[])
                let bindedVProperties () =
                    ([],0)
                    |> Style.PropagateStyle style fakeTag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = Tag.NodeContainer(Canvas,defaultArg Children [])
                      Properties = vprops   |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node

            (*** ****************** ***) 
            (***      Ellipse       ***) 
            (*** ****************** ***) 
            static member ellipse( ?style : Style ) =
                                     
                let tag = Tag.NodeSingle (TaggedControl Ellipse)
                let bindedVProperties () =
                    ([],0)
                    |> Style.PropagateStyle style tag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = tag
                      Properties = vprops   |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node



            (*** ****************** ***) 
            (***      Line          ***) 
            (*** ****************** ***) 
            static member line( ?Fill               : Color
                               ,?Stroke             : Color
                               ,?StrokeThickness    : float
                               ,?Coordinate         : LineCoordinate
                               ,?style              : Style ) =
                                     
                let tag = Tag.NodeSingle (TaggedControl Line)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties  Fill            (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.Fill x)))
                    |> bindVProperties  Stroke          (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.Stroke x)))
                    |> bindVProperties  StrokeThickness (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.StrokeThickness x)))
                    |> bindVProperties  Coordinate      (fun x -> VProperty.FEProperty (ShapeProperty (LineProperty (LineProperty.LineCoordinate x))))
                    |> Style.PropagateStyle style tag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = tag
                      Properties = vprops   |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node

            (*** ****************** ***) 
            (***      PolyLine      ***) 
            (*** ****************** ***) 
            static member polyline( ?Fill               : Color
                                   ,?Stroke             : Color
                                   ,?StrokeThickness    : float
                                   ,?Points             : Coordinate list
                                   ,?style              : Style ) =
                                     
                let tag = Tag.NodeSingle (TaggedControl Polyline)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties  Fill            (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.Fill x)))
                    |> bindVProperties  Stroke          (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.Stroke x)))
                    |> bindVProperties  StrokeThickness (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.StrokeThickness x)))
                    |> bindVProperties  Points          (fun x -> VProperty.FEProperty (ShapeProperty (PolyLineProperty (PolyLineProperty.Points x))))
                    |> Style.PropagateStyle style tag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = tag
                      Properties = vprops   |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node

            (*** ****************** ***) 
            (***      Rectangle     ***) 
            (*** ****************** ***) 
            static member rectangle( ?Fill              : Color
                                    ,?Stroke            : Color
                                    ,?StrokeThickness   : float
                                    ,?Radius            : Radius
                                    ,?style             : Style ) =
                                     
                let tag = Tag.NodeSingle (TaggedControl Rectangle)
                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties  Fill            (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.Fill x)))
                    |> bindVProperties  Stroke          (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.Stroke x)))
                    |> bindVProperties  StrokeThickness (fun x -> VProperty.FEProperty (ShapeProperty (ShapeProperty.StrokeThickness x)))
                    |> bindVProperties  Radius          (fun x -> VProperty.FEProperty (ShapeProperty (RectangleProperty (RectangleProperty.Radius x))))
                    |> Style.PropagateStyle style tag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = tag
                      Properties = vprops   |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node


            (*** ****************** ***) 
            (***      Border        ***) 
            (*** ****************** ***) 
            static member border( ?style           : Style ) =
                                     
                let tag = Tag.NodeSingle (TaggedBorder Border)
                let bindedVProperties () =
                    ([],0)
                    |> Style.PropagateStyle style tag 
                    |> fst

                let vprops  = bindedVProperties ()
                let node =
                    { Tag        = tag
                      Properties = vprops   |> VProperties
                      WPFEvents  = [] |> WPFEvents 
                      Events     = [] |> VEvents  }
                WPFTree node


            (*** ****************** ***) 
            (***      Window        ***) 
            (*** ****************** ***) 
            static member window( children              : WPFTree<'Msg>
                                 ,?WindowStyle          : WindowStyle 
                                 ,?WindowState          : WindowState 
                                 ,?Title                : string 
                                 ,?ResizeMode           : ResizeMode 
                                 ,?AllowsTransparency   : bool 
                                 ,?Activated            : EventArgs -> 'Msg 
                                 ,?Closed               : EventArgs -> 'Msg
                                 ,?Closing              : CancelEventArgs -> 'Msg
                                 ,?Deactivated          : EventArgs -> 'Msg
                                 ,?Loaded               : RoutedEventArgs -> 'Msg  
                                 ,?style                : Style) =

                // This is a fake tag to allow style construction for a window 
                // a window is a control like a button
                let fakeTag = Tag.NodeSingle (TaggedControl TaggedControl.Button)

                let bindedVProperties () =
                    ([],0)
                    |> bindVProperties  WindowStyle         (fun x -> VProperty.WindowProperty (WindowProperty.WindowStyle x))
                    |> bindVProperties  WindowState         (fun x -> VProperty.WindowProperty (WindowProperty.WindowState x))
                    |> bindVProperties  Title               (fun x -> VProperty.WindowProperty (WindowProperty.Title x))
                    |> bindVProperties  ResizeMode          (fun x -> VProperty.WindowProperty (WindowProperty.ResizeMode x))
                    |> bindVProperties  AllowsTransparency  (fun x -> VProperty.WindowProperty (WindowProperty.AllowsTransparency x))         
                    |> Style.PropagateStyle style fakeTag 
                    |> fst

                let bindedVEvents () =
                    ([],0)
                    |> bindWPFEvents Activated   (fun x -> new EventHandler(x))         WPFActivated   
                    |> bindWPFEvents Closed      (fun x -> new EventHandler(x))         WPFClosed      
                    |> bindWPFEvents Closing     (fun x -> new CancelEventHandler(x))   WPFClosing  
                    |> bindWPFEvents Deactivated (fun x -> new EventHandler(x))         WPFDeactivated 
                    |> bindWPFEvents Loaded      (fun x -> new RoutedEventHandler(x))   WPFLoaded      
                    |> fst

                let vprops  = bindedVProperties ()
                let vevents = bindedVEvents ()
                { Tree = children
                  Properties = vprops   |> VProperties
                  WPFEvents  = vevents |> WPFEvents 
                  Events     = [] |> VEvents  }

    
    