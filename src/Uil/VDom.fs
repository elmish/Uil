namespace Uil


module VDom =
    open System.Windows
    open System.Windows.Media


    module VirtualProperty =
        open System.Windows.Markup
        open System.Windows.Controls
        open System.Windows.Controls.Primitives

        module FsWPFRepresentation =
            type ColDef = 
                { Width : int
                  Unit  : GridUnitType }

            type RowDef = 
                { Height : int
                  Unit   : GridUnitType }

            type LineCoordinate =
                {   X1 : float
                    Y1 : float
                    X2 : float
                    Y2 : float  }

            type Coordinate =
                {   X : float
                    Y : float   }
                
            type Radius =
                {   X : float
                    Y : float   }

        open FsWPFRepresentation

        type UIElementStyle =
            | Column        of int 
            | ColumnSpan    of int 
            | Row           of int 
            | RowSpan       of int 
            | Bottom        of float
            | Left          of float
            | Right         of float
            | Top           of float
            | IsEnabled     of bool
            | Opacity       of float
            | Visibility    of Visibility

        type FrameworkElementStyle = 
            | Height                of float
            | HorizontalAlignment   of HorizontalAlignment
            | Margin                of Thickness
            | VerticalAlignment     of VerticalAlignment
            | Width                 of float
            
        type ControlStyle =
            | Background                    of Color
            | BorderBrush                   of Color
            | BorderThickness               of Thickness
            | FontFamily                    of FontFamily
            | FontSize                      of float
            | FontStretch                   of FontStretch
            | FontStyle                     of FontStyle
            | FontWeight                    of FontWeight
            | Foreground                    of Color
            | HorizontalContentAlignment    of HorizontalAlignment
            | Padding                       of Thickness
            | VerticalContentAlignment      of VerticalAlignment
                        
        type PanelStyle =
            | Background of Color

        type TextBlockStyle =
            | Background                    of Color
            | FontFamily                    of FontFamily
            | FontSize                      of float
            | FontStretch                   of FontStretch
            | FontStyle                     of FontStyle
            | FontWeight                    of FontWeight
            | Foreground                    of Color
            | Padding                       of Thickness

        type BorderStyle =
            | Background of Color
            | BorderBrush                   of Color
            | BorderThickness               of Thickness

        type DecoratorStyle =
            | BorderStyle of BorderStyle
       
        type VStyle = 
            | UIElementStyle of UIElementStyle
            | FrameworkElementStyle of FrameworkElementStyle
            | ControlStyle of ControlStyle
            | PanelStyle of PanelStyle
            | TextBlockStyle of TextBlockStyle
            | DecoratorStyle of DecoratorStyle

        type ProgressProperty =
            | IsIndeterminate of bool
            | Orientation of Orientation

        type SliderProperty =
            | TickFrequency of float
            | TickPlacement of TickPlacement
            | Ticks of DoubleCollection
            | AutoToolTipPlacement of AutoToolTipPlacement
            | AutoToolTipPrecision of int

        type RangeProperty =
            | ProgressProperty of ProgressProperty
            | SliderProperty of SliderProperty
            | LargeChange of float
            | Minimum of float
            | Maximum of float
            | SmallChange of float
            | Value of float

        type TextBoxProperty =
            | CanEnter of bool
            | CanTab of bool
            | Language of XmlLanguage
            | SpellCheck of bool
            | Text of string
            | TextAlignment of TextAlignment
            | TextDecorations of TextDecorationCollection
            | TextWrapping of TextWrapping
            | VerticalScrollBarVisibility of ScrollBarVisibility

                
        type ToggleButtonProperty =
            | IsChecked of bool
            | IsThreeState of bool

        type ButtonBaseProperty =
            | ToggleButtonProperty of ToggleButtonProperty

        type SingleContentProperty =
            | ButtonBaseProperty of ButtonBaseProperty
            | Content of obj

        type ComboBoxProperty =
            | IsEditable of bool
            | Text of string

        type SelectableProperty =
            | ComboBoxProperty of ComboBoxProperty
            | SelectedItem of obj

        type CollectionContentProperty =
            | SelectableProperty of SelectableProperty
            | IsTextSearchEnabled of bool
            | IsTextSearchCaseSensitive of bool
            // TODO : Add a new tag and change WPFTree structure
            //| Items of ItemCollection

        type ControlProperty =
            | RangeProperty of RangeProperty
            | TextBoxProperty of TextBoxProperty
            | SingleContentProperty of SingleContentProperty
            | CollectionContentProperty of CollectionContentProperty
        


        type StackPanelProperty =
            | CanHorizontallyScroll of bool
            | CanVerticallyScroll   of bool
            | Orientation of Orientation

        type GridProperty =
            | ColumnDefinitions   of ColDef list
            | RowDefinitions      of RowDef list

        type PanelProperty =
            | GridProperty of GridProperty
            | StackPanelProperty of StackPanelProperty

        
        type TextBlockProperty =
            | Text of string
            | TextAlignment of TextAlignment
            | TextTrimming of TextTrimming 
            | TextWrapping of TextWrapping

        type LineProperty =
            | LineCoordinate of LineCoordinate

        type PolyLineProperty =
            | Points of Coordinate list

        type RectangleProperty =
            | Radius of Radius

        type ShapeProperty =
            | Fill of Color
            | Stroke of Color
            | StrokeThickness of float
            | LineProperty of LineProperty
            | PolyLineProperty of PolyLineProperty
            | RectangleProperty of RectangleProperty

        type FEProperty =
            | ControlProperty of ControlProperty
            | PanelProperty of PanelProperty
            | ShapeProperty of ShapeProperty
            | TextBlockProperty of TextBlockProperty

        type WindowProperty =
            | WindowStyle         of WindowStyle
            | WindowState         of WindowState
            | Title               of string
            | ResizeMode          of ResizeMode
            | AllowsTransparency  of bool 
        
        type VProperty =
            | VStyle            of VStyle
            | WindowProperty    of WindowProperty
            | FEProperty        of FEProperty



    module VirtualEvent =
        open System
        open System.ComponentModel
        open System.Windows.Controls

        type TextBoxEvent  =
            | TextChanged of TextChangedEventHandler
                
        type ToggleButtonEvent =
            | Checked       of RoutedEventHandler
            | Unchecked     of RoutedEventHandler
            | Indeterminate of RoutedEventHandler

        type ButtonBaseEvent =
            | Click of RoutedEventHandler
            | ToggleButtonEvent of ToggleButtonEvent

        type SingleContentEvent =
            | ButtonBaseEvent of ButtonBaseEvent

        type SelectableEvent =
            | SelectionChanged of SelectionChangedEventHandler 

        type CollectionContentEvent =
            | SelectableEvent of SelectableEvent

        type RangeBaseEvent =
            | ValueChanged of RoutedPropertyChangedEventHandler<double>

        type ControlEvent =
            | RangeBaseEvent of RangeBaseEvent
            | TextBoxEvent of TextBoxEvent 
            | SingleContentEvent of SingleContentEvent
            | CollectionContentEvent of CollectionContentEvent
        
        type FEEvent =
            | ControlEvent of ControlEvent

        type WindowEvent =
            | Activated     of EventHandler
            | Closed        of EventHandler
            | Closing       of CancelEventHandler
            | Deactivated   of EventHandler
            | Loaded        of RoutedEventHandler

        type VEvent =
            | NoEvent 
            | WindowEvent of WindowEvent
            | FEEvent of FEEvent 


    module VDomTypes =
        open System
        open System.ComponentModel
        open VirtualProperty
        open VirtualEvent
        open System.Windows.Controls




        type VProperties = VProperties of (VProperty*int) list  
        
        type VEvents = VEvents of (VEvent*int) list              // Built from TmpEvents with the use of the dispatcher hidden from the user

        type WPFLambda<'Msg,'args,'handler> = ('args -> 'Msg) * ((obj -> 'args -> unit) -> 'handler)

        type WPFEvent<'Msg> = 
            | WPFClick              of WPFLambda<'Msg,RoutedEventArgs,RoutedEventHandler>
            | WPFTextChanged        of WPFLambda<'Msg,TextChangedEventArgs,TextChangedEventHandler> 
            | WPFActivated          of WPFLambda<'Msg,EventArgs,EventHandler>
            | WPFClosed             of WPFLambda<'Msg,EventArgs,EventHandler>
            | WPFClosing            of WPFLambda<'Msg,CancelEventArgs,CancelEventHandler>
            | WPFDeactivated        of WPFLambda<'Msg,EventArgs,EventHandler>
            | WPFLoaded             of WPFLambda<'Msg,RoutedEventArgs,RoutedEventHandler>
            | WPFChecked            of WPFLambda<'Msg,RoutedEventArgs,RoutedEventHandler>
            | WPFUnchecked          of WPFLambda<'Msg,RoutedEventArgs,RoutedEventHandler>
            | WPFIndeterminate      of WPFLambda<'Msg,RoutedEventArgs,RoutedEventHandler>
            | WPFValueChanged       of WPFLambda<'Msg,RoutedPropertyChangedEventArgs<double>,RoutedPropertyChangedEventHandler<double>>
            | WPFSelectionChanged   of WPFLambda<'Msg,SelectionChangedEventArgs,SelectionChangedEventHandler>

        type WPFEvents<'Msg> = WPFEvents of (WPFEvent<'Msg>*int) list    // Temporary, making the events independent of the dispatcher


        
        type TaggedContainer =
            | Grid
            | StackPanel
            | Canvas
            
        type TaggedControl  =
            | Button 
            | ToggleButton
            | CheckBox
            | RadioButton
            | ProgressBar
            | TextBox 
            | Slider
            | Line
            | Rectangle
            | Polyline
            | Ellipse

        
        type TaggedTextBlock =
            | TextBlock
        
        type TaggedBorder =
            | Border

        type TaggedSingle =
            | TaggedControl of TaggedControl
            | TaggedTextBlock of TaggedTextBlock
            | TaggedBorder of TaggedBorder 

        type TaggedItems =
            | ComboBox
    
        type Tag<'Msg> =
            | NodeContainer of TaggedContainer * WPFTree<'Msg> list
            | NodeItems     of TaggedItems * WPFTree<'Msg> list
            | NodeSingle    of TaggedSingle

        and WPFNodeElement<'Msg>  = 
            { Tag : Tag<'Msg>
              Properties : VProperties
              Events : VEvents 
              WPFEvents : WPFEvents<'Msg>   }

        and WPFTree<'Msg>  = WPFTree of WPFNodeElement<'Msg>

        type WPFWindow<'Msg>  = 
            { Properties : VProperties
              Events : VEvents 
              WPFEvents : WPFEvents<'Msg>  
              Tree : WPFTree<'Msg> }


    module VirtualPropertyDefaultValues =
        open System
        open VirtualProperty
        open FsWPFRepresentation
        open System.Windows.Controls
        open System.Windows.Controls.Primitives
        open System.Windows.Markup

        // TODO : Specify/Document default values

        // The default value taken by WPF framework might not be the default value applied by this DSL

        type UIElementStyle with    
            member x.DefaultValue =
                match x with
                | Column     _  -> Column       0
                | ColumnSpan _  -> ColumnSpan   1
                | Row        _  -> Row          0
                | RowSpan    _  -> RowSpan      1
                | Bottom     _  -> Bottom       0.
                | Left       _  -> Left         0.
                | Right      _  -> Right        0.
                | Top        _  -> Top          0.
                | UIElementStyle.IsEnabled  _  -> UIElementStyle.IsEnabled    true
                | Opacity    _  -> Opacity      1.0  
                | Visibility _  -> Visibility   Visibility.Visible 
        
        type FrameworkElementStyle with
            member x.DefaultValue =
                match x with
                | Height                _   -> Height              nan
                | HorizontalAlignment   _   -> HorizontalAlignment HorizontalAlignment.Stretch 
                | Margin                _   -> Margin              (new Thickness(0.,0.,0.,0.))
                | VerticalAlignment     _   -> VerticalAlignment   VerticalAlignment.Stretch
                | Width                 _   -> Width               nan

        type ControlStyle with
            member x.DefaultValue =
                match x with
                | ControlStyle.Background       _ -> ControlStyle.Background        Colors.Transparent
                | ControlStyle.BorderBrush      _ -> ControlStyle.BorderBrush       Colors.Transparent
                | ControlStyle.BorderThickness  _ -> ControlStyle.BorderThickness   (new Thickness(0.,0.,0.,0.))
                | ControlStyle.FontFamily       _ -> ControlStyle.FontFamily        (Media.FontFamily("Segoe UI"))
                | ControlStyle.FontSize         _ -> ControlStyle.FontSize          (SystemFonts.MessageFontSize)
                | ControlStyle.FontStretch      _ -> ControlStyle.FontStretch       FontStretches.Normal
                | ControlStyle.FontStyle        _ -> ControlStyle.FontStyle         FontStyles.Normal
                | ControlStyle.FontWeight       _ -> ControlStyle.FontWeight        FontWeights.Normal
                | ControlStyle.Foreground       _ -> ControlStyle.Foreground        Colors.Black
                | HorizontalContentAlignment    _ -> HorizontalContentAlignment     HorizontalAlignment.Left
                | ControlStyle.Padding          _ -> ControlStyle.Padding           (new Thickness(0.,0.,0.,0.))
                | VerticalContentAlignment      _ -> VerticalContentAlignment       VerticalAlignment.Top

        type PanelStyle with
            member x.DefaultValue =
                match x with
                | PanelStyle.Background _   -> PanelStyle.Background    Colors.Transparent

        type TextBlockStyle with
            member x.DefaultValue =
                match x with
                | TextBlockStyle.Background     _ -> TextBlockStyle.Background    Colors.Transparent
                | TextBlockStyle.FontFamily     _ -> TextBlockStyle.FontFamily    (Media.FontFamily("Segoe UI"))
                | TextBlockStyle.FontSize       _ -> TextBlockStyle.FontSize      (SystemFonts.MessageFontSize)
                | TextBlockStyle.FontStretch    _ -> TextBlockStyle.FontStretch   FontStretches.Normal
                | TextBlockStyle.FontStyle      _ -> TextBlockStyle.FontStyle     FontStyles.Normal
                | TextBlockStyle.FontWeight     _ -> TextBlockStyle.FontWeight    FontWeights.Normal
                | TextBlockStyle.Foreground     _ -> TextBlockStyle.Foreground    Colors.Black
                | TextBlockStyle.Padding        _ -> TextBlockStyle.Padding       (new Thickness(0.,0.,0.,0.))

        type BorderStyle with
            member x.DefaultValue =
                match x with
                | BorderStyle.Background        _ -> BorderStyle.Background Colors.Transparent
                | BorderStyle.BorderBrush       _ -> BorderStyle.BorderBrush Colors.Transparent
                | BorderStyle.BorderThickness   _ -> BorderStyle.BorderThickness (new Thickness(0.,0.,0.,0.))

        type DecoratorStyle with
            member x.DefaultValue =
                match x with
                | BorderStyle bStyle -> bStyle.DefaultValue |> BorderStyle

        type VStyle with
            member x.DefaultValue =
                match x with
                | UIElementStyle uieStyle       -> uieStyle.DefaultValue |> UIElementStyle
                | FrameworkElementStyle feStyle -> feStyle.DefaultValue  |> FrameworkElementStyle
                | ControlStyle cStyle           -> cStyle.DefaultValue   |> ControlStyle
                | PanelStyle pStyle             -> pStyle.DefaultValue   |> PanelStyle 
                | TextBlockStyle tbStyle        -> tbStyle.DefaultValue  |> TextBlockStyle
                | DecoratorStyle dStyle         -> dStyle.DefaultValue   |> DecoratorStyle


        type ProgressProperty with
            member x.DefaultValue =
                match x with
                | IsIndeterminate               _ -> IsIndeterminate              false
                | ProgressProperty.Orientation  _ -> ProgressProperty.Orientation Orientation.Horizontal

        type SliderProperty  with
            member x.DefaultValue =
                match x with
                | TickFrequency         _ -> TickFrequency                          1.0
                | TickPlacement         _ -> SliderProperty.TickPlacement           TickPlacement.None 
                | Ticks                 _ -> Ticks                                  null
                | AutoToolTipPlacement  _ -> SliderProperty.AutoToolTipPlacement    AutoToolTipPlacement.None 
                | AutoToolTipPrecision  _ -> SliderProperty.AutoToolTipPrecision    0

        type RangeProperty with
            member x.DefaultValue =
                match x with
                | ProgressProperty pp -> pp.DefaultValue |> ProgressProperty
                | SliderProperty   sp -> sp.DefaultValue |> SliderProperty
                | LargeChange      _  -> LargeChange 1.0
                | Minimum          _  -> Minimum     0.0
                | Maximum          _  -> Maximum     100.0
                | SmallChange      _  -> SmallChange 0.1
                | Value            _  -> Value       0.0

        type TextBoxProperty with
            member x.DefaultValue =
                match x with
                | CanEnter                      _ -> CanEnter                   false   
                | CanTab                        _ -> CanTab                     false   
                | Language                      _ -> Language                   (XmlLanguage.GetLanguage("en-US"))  
                | SpellCheck                    _ -> TextBoxProperty.SpellCheck false                 
                | TextBoxProperty.Text                          _ -> TextBoxProperty.Text                           ""
                | TextBoxProperty.TextAlignment                 _ -> TextBoxProperty.TextAlignment                  TextAlignment.Left
                | TextBoxProperty.TextDecorations               _ -> TextBoxProperty.TextDecorations                null
                | TextBoxProperty.TextWrapping                  _ -> TextBoxProperty.TextWrapping                   TextWrapping.NoWrap
                | TextBoxProperty.VerticalScrollBarVisibility   _ -> TextBoxProperty.VerticalScrollBarVisibility    ScrollBarVisibility.Hidden
                
        type ToggleButtonProperty with
            member x.DefaultValue =
                match x with
                | IsChecked     _ -> IsChecked      false
                | IsThreeState  _ -> IsThreeState   false

        type ButtonBaseProperty with
            member x.DefaultValue =
                match x with
                | ToggleButtonProperty tbp -> tbp.DefaultValue |> ToggleButtonProperty

        type SingleContentProperty with
            member x.DefaultValue =
                match x with
                | ButtonBaseProperty bbp -> bbp.DefaultValue |> ButtonBaseProperty
                | Content            _   -> Content null

        type ComboBoxProperty with
            member x.DefaultValue =
                match x with
                | IsEditable            _ -> IsEditable             false
                | ComboBoxProperty.Text _ -> ComboBoxProperty.Text  ""

        type SelectableProperty with
            member x.DefaultValue =
                match x with 
                | ComboBoxProperty        cbp -> cbp.DefaultValue |> ComboBoxProperty
                | SelectedItem              _ -> SelectedItem null


        type CollectionContentProperty with
            member x.DefaultValue =
                match x with 
                | SelectableProperty        sp -> sp.DefaultValue |> SelectableProperty
                | IsTextSearchEnabled       _  -> IsTextSearchEnabled       false
                | IsTextSearchCaseSensitive _  -> IsTextSearchCaseSensitive false

        type ControlProperty with
            member x.DefaultValue =
                match x with 
                | RangeProperty             rp  -> rp.DefaultValue  |> RangeProperty
                | TextBoxProperty           tbp -> tbp.DefaultValue |> TextBoxProperty
                | SingleContentProperty     scp -> scp.DefaultValue |> SingleContentProperty
                | CollectionContentProperty ccp -> ccp.DefaultValue |> CollectionContentProperty
        
        type StackPanelProperty with
            member x.DefaultValue =
                match x with 
                | CanHorizontallyScroll _ -> CanHorizontallyScroll false
                | CanVerticallyScroll   _ -> CanVerticallyScroll false
                | StackPanelProperty.Orientation _ -> StackPanelProperty.Orientation (Orientation.Vertical)

        type GridProperty with
            member x.DefaultValue =
                match x with 
                | ColumnDefinitions   _ -> ColumnDefinitions []
                | RowDefinitions      _ -> RowDefinitions []

        type PanelProperty with
            member x.DefaultValue =
                match x with 
                | GridProperty          gp  -> gp.DefaultValue  |> GridProperty
                | StackPanelProperty    spp -> spp.DefaultValue |> StackPanelProperty

        
        type TextBlockProperty with
            member x.DefaultValue =
                match x with 
                | Text          _ -> Text         ""
                | TextAlignment _ -> TextAlignment (TextAlignment.Left)
                | TextTrimming  _ -> TextTrimming (TextTrimming.None)
                | TextWrapping  _ -> TextWrapping (TextWrapping.NoWrap)


        type LineProperty with
            member x.DefaultValue =
                match x with 
                | LineCoordinate _ -> {X1 = 0.0 ; Y1 = 0.0 ; X2 = 0.0 ; Y2 = 0.0} |> LineCoordinate

        type PolyLineProperty with
            member x.DefaultValue =
                match x with 
                | Points _ -> [] |> Points

        type RectangleProperty with
            member x.DefaultValue =
                match x with 
                | Radius _ -> {X = 0.0 ; Y = 0.0 } |> Radius

        type ShapeProperty with
            member x.DefaultValue =
                match x with 
                | Fill              _   -> Fill            Colors.Transparent               
                | Stroke            _   -> Stroke          Colors.Transparent
                | StrokeThickness   _   -> StrokeThickness 0.
                | LineProperty      lp  -> lp.DefaultValue  |> LineProperty
                | PolyLineProperty  plp -> plp.DefaultValue |> PolyLineProperty
                | RectangleProperty rp  -> rp.DefaultValue  |> RectangleProperty


        type FEProperty with
            member x.DefaultValue =
                match x with 
                | ControlProperty   cp  -> cp.DefaultValue |> ControlProperty
                | PanelProperty     pp  -> pp.DefaultValue |> PanelProperty
                | TextBlockProperty tbp -> tbp.DefaultValue |> TextBlockProperty
                | ShapeProperty     sp  -> sp.DefaultValue |> ShapeProperty

        type WindowProperty with
            member x.DefaultValue =
                match x with 
                | WindowStyle         _ ->  WindowStyle         WindowStyle.SingleBorderWindow  
                | WindowState         _ ->  WindowState         WindowState.Normal
                | Title               _ ->  Title               ""
                | ResizeMode          _ ->  ResizeMode          ResizeMode.CanResize 
                | AllowsTransparency  _ ->  AllowsTransparency  false
        
        type VProperty with
            member x.DefaultValue =
                match x with 
                | VStyle            vs  -> vs.DefaultValue |> VStyle
                | WindowProperty    wp  -> wp.DefaultValue |> WindowProperty
                | FEProperty        fep -> fep.DefaultValue |> FEProperty


    module VDomExt =
        open System.Windows.Controls
        open VDomTypes
        open System
        open System.Windows.Controls.Primitives

        
        (*** ********************************** ***)
        (***         Convert Module             ***)
        (*** ********************************** ***)
        /// Module which contains extension to convert
        /// F# WPF representation types to real WPF Types
        module Convert =
            open VirtualProperty
            open FsWPFRepresentation

            type ColDef with
                member x.Convert() = 
                    let cd = new ColumnDefinition()
                    let width = new GridLength(x.Width |> float,x.Unit)
                    cd.Width <- width
                    cd

            type RowDef with
                member x.Convert() = 
                    let rd = new RowDefinition()
                    let height = new GridLength(x.Height |> float,x.Unit)
                    rd.Height <- height
                    rd
       

        (*** ********************************** ***)
        (***         VPropertyUpdate Module     ***)
        (*** ********************************** ***)
        /// Module which contains extension to update
        /// WPF object depending on the virtual property value
        module VPropertyUpdate =   
            open Convert
            open VirtualProperty
            open System.Windows.Shapes

            type WPFObjectUpdate =
                | UIElementUpdate           of (UIElement -> unit)
                | FrameworkElementUpdate    of (FrameworkElement -> unit)
                | ControlUpdate             of (Control -> unit)
                | PanelUpdate               of (Panel -> unit)
                | RangeBaseUpdate           of (RangeBase -> unit)
                | ProgressBarUpdate         of (ProgressBar -> unit)
                | SliderUpdate              of (Slider -> unit)
                | TextBoxUpdate             of (TextBox -> unit)
                | ToggleButtonUpdate        of (ToggleButton -> unit)
                | ButtonBaseUpdate          of (ButtonBase -> unit)
                | SingleContentUpdate       of (ContentControl -> unit)
                | ComboBoxUpdate            of (ComboBox -> unit)
                | SelectableUpdate          of (Selector -> unit)
                | CollectionContentUpdate   of (ItemsControl -> unit)
                | StackPanelUpdate          of (StackPanel -> unit)
                | TextBlockUpdate           of (TextBlock -> unit)
                | GridUpdate                of (Grid -> unit)
                | WindowUpdate              of (Window -> unit)
                | ShapeUpdate               of (Shape -> unit)
                | LineUpdate                of (Line -> unit)
                | PolyLineUpdate            of (Polyline -> unit)
                | RectangeUpdate            of (Rectangle -> unit)
                | BorderUpdate              of (Border -> unit)

            type UIElementStyle with
                member x.PropertyUpdate() =
                    match x with
                    | Column     prop   -> UIElementUpdate (fun ui -> Grid.SetColumn(ui,prop))
                    | ColumnSpan prop   -> UIElementUpdate (fun ui -> Grid.SetColumnSpan(ui,prop))
                    | Row        prop   -> UIElementUpdate (fun ui -> Grid.SetRow(ui,prop))
                    | RowSpan    prop   -> UIElementUpdate (fun ui -> Grid.SetRowSpan(ui,prop)) 
                    | Bottom     prop   -> UIElementUpdate (fun ui -> Canvas.SetBottom(ui,prop))
                    | Left       prop   -> UIElementUpdate (fun ui -> Canvas.SetLeft(ui,prop))
                    | Right      prop   -> UIElementUpdate (fun ui -> Canvas.SetRight(ui,prop))
                    | Top        prop   -> UIElementUpdate (fun ui -> Canvas.SetTop(ui,prop))
                    | UIElementStyle.IsEnabled  prop    -> UIElementUpdate (fun ui -> ui.IsEnabled <- prop) 
                    | Opacity    prop   -> UIElementUpdate (fun ui -> ui.Opacity <- prop) 
                    | Visibility prop   -> UIElementUpdate (fun ui -> ui.Visibility <- prop) 
            
            type FrameworkElementStyle with
                member x.PropertyUpdate() =
                    match x with
                    | Height                prop  -> FrameworkElementUpdate (fun fw -> fw.Height <- prop)
                    | HorizontalAlignment   prop  -> FrameworkElementUpdate (fun fw -> fw.HorizontalAlignment <- prop)
                    | Margin                prop  -> FrameworkElementUpdate (fun fw -> fw.Margin <- prop)
                    | VerticalAlignment     prop  -> FrameworkElementUpdate (fun fw -> fw.VerticalAlignment <- prop)
                    | Width                 prop  -> FrameworkElementUpdate (fun fw -> fw.Width <- prop)

            type ControlStyle with
                member x.PropertyUpdate() =
                    match x with
                    | ControlStyle.Background       prop  -> ControlUpdate (fun c -> c.Background <- new SolidColorBrush(prop))
                    | ControlStyle.BorderBrush      prop  -> ControlUpdate (fun c -> c.BorderBrush <- new SolidColorBrush(prop))
                    | ControlStyle.BorderThickness  prop  -> ControlUpdate (fun c -> c.BorderThickness <- prop)
                    | ControlStyle.FontFamily       prop  -> ControlUpdate (fun c -> c.FontFamily <- prop)
                    | ControlStyle.FontSize         prop  -> ControlUpdate (fun c -> c.FontSize <- prop)
                    | ControlStyle.FontStretch      prop  -> ControlUpdate (fun c -> c.FontStretch <- prop)
                    | ControlStyle.FontStyle        prop  -> ControlUpdate (fun c -> c.FontStyle <- prop)
                    | ControlStyle.FontWeight       prop  -> ControlUpdate (fun c -> c.FontWeight <- prop)
                    | ControlStyle.Foreground       prop  -> ControlUpdate (fun c -> c.Foreground <- new SolidColorBrush(prop))
                    | HorizontalContentAlignment    prop  -> ControlUpdate (fun c -> c.HorizontalContentAlignment <- prop)
                    | ControlStyle.Padding          prop  -> ControlUpdate (fun c -> c.Padding <- prop)
                    | VerticalContentAlignment      prop  -> ControlUpdate (fun c -> c.VerticalContentAlignment <- prop)

            type PanelStyle with
                member x.PropertyUpdate() =
                    match x with
                    | PanelStyle.Background     prop -> PanelUpdate (fun p -> p.Background <- new SolidColorBrush(prop))

            type TextBlockStyle with
                member x.PropertyUpdate() =
                    match x with
                    | TextBlockStyle.Background     prop  -> TextBlockUpdate (fun c -> c.Background <- new SolidColorBrush(prop))
                    | TextBlockStyle.FontFamily     prop  -> TextBlockUpdate (fun c -> c.FontFamily <- prop)
                    | TextBlockStyle.FontSize       prop  -> TextBlockUpdate (fun c -> c.FontSize <- prop)
                    | TextBlockStyle.FontStretch    prop  -> TextBlockUpdate (fun c -> c.FontStretch <- prop)
                    | TextBlockStyle.FontStyle      prop  -> TextBlockUpdate (fun c -> c.FontStyle <- prop)
                    | TextBlockStyle.FontWeight     prop  -> TextBlockUpdate (fun c -> c.FontWeight <- prop)
                    | TextBlockStyle.Foreground     prop  -> TextBlockUpdate (fun c -> c.Foreground <- new SolidColorBrush(prop))
                    | TextBlockStyle.Padding        prop  -> TextBlockUpdate (fun c -> c.Padding <- prop)

            type BorderStyle with
                member x.PropertyUpdate() =
                    match x with
                    | BorderStyle.Background        prop  -> BorderUpdate (fun b -> b.Background <- new SolidColorBrush(prop))
                    | BorderStyle.BorderBrush       prop  -> BorderUpdate (fun b -> b.BorderBrush <- new SolidColorBrush(prop))
                    | BorderStyle.BorderThickness   prop  -> BorderUpdate (fun b -> b.BorderThickness <- prop)

            type DecoratorStyle with
                member x.PropertyUpdate() =
                    match x with
                    | BorderStyle bStyle -> bStyle.PropertyUpdate() 

            type VStyle with
                member x.PropertyUpdate() =
                    match x with
                    | UIElementStyle uieStyle       -> uieStyle.PropertyUpdate()
                    | FrameworkElementStyle feStyle -> feStyle.PropertyUpdate()
                    | ControlStyle cStyle           -> cStyle.PropertyUpdate()
                    | PanelStyle pStyle             -> pStyle.PropertyUpdate()
                    | TextBlockStyle tbStyle        -> tbStyle.PropertyUpdate()
                    | DecoratorStyle dStyle         -> dStyle.PropertyUpdate()

            type ProgressProperty with
                member x.PropertyUpdate() =
                    match x with
                    | IsIndeterminate               prop -> ProgressBarUpdate( fun pb -> pb.IsIndeterminate <- prop)
                    | ProgressProperty.Orientation  prop -> ProgressBarUpdate( fun pb -> pb.Orientation <- prop)

            type SliderProperty  with
                member x.PropertyUpdate() =
                    match x with
                    | TickFrequency         prop -> SliderUpdate( fun s -> s.TickFrequency <- prop)
                    | TickPlacement         prop -> SliderUpdate( fun s -> s.TickPlacement <- prop)
                    | Ticks                 prop -> SliderUpdate( fun s -> s.Ticks <- prop)
                    | AutoToolTipPlacement  prop -> SliderUpdate( fun s -> s.AutoToolTipPlacement <- prop)
                    | AutoToolTipPrecision  prop -> SliderUpdate( fun s -> s.AutoToolTipPrecision <- prop)

            type RangeProperty with
                member x.PropertyUpdate() =
                    match x with
                    | ProgressProperty pp -> pp.PropertyUpdate()
                    | SliderProperty   sp -> sp.PropertyUpdate()
                    | LargeChange      prop -> RangeBaseUpdate( fun rb -> rb.LargeChange <- prop)
                    | Minimum          prop -> RangeBaseUpdate( fun rb -> rb.Minimum <- prop)
                    | Maximum          prop -> RangeBaseUpdate( fun rb -> rb.Maximum <- prop)
                    | SmallChange      prop -> RangeBaseUpdate( fun rb -> rb.SmallChange <- prop)
                    | Value            prop -> RangeBaseUpdate( fun rb -> rb.Value <- prop)

            type TextBoxProperty with
                member x.PropertyUpdate() =
                    match x with
                    | CanEnter                      prop -> TextBoxUpdate( fun tb -> tb.AcceptsReturn <- prop)
                    | CanTab                        prop -> TextBoxUpdate( fun tb -> tb.AcceptsTab <- prop)
                    | Language                      prop -> TextBoxUpdate( fun tb -> tb.Language <- prop)
                    | SpellCheck                    prop -> TextBoxUpdate( fun tb -> tb.SpellCheck.IsEnabled <- prop)
                    | TextBoxProperty.Text                              prop -> TextBoxUpdate( fun tb -> tb.Text <- prop)
                    | TextBoxProperty.TextAlignment                     prop -> TextBoxUpdate( fun tb -> tb.TextAlignment <- prop)
                    | TextBoxProperty.TextDecorations                   prop -> TextBoxUpdate( fun tb -> tb.TextDecorations <- prop)
                    | TextBoxProperty.TextWrapping                      prop -> TextBoxUpdate( fun tb -> tb.TextWrapping <- prop)
                    | TextBoxProperty.VerticalScrollBarVisibility       prop -> TextBoxUpdate( fun tb -> tb.VerticalScrollBarVisibility <- prop)

            type ToggleButtonProperty with
                member x.PropertyUpdate() =
                    match x with
                    | IsChecked     prop -> ToggleButtonUpdate ( fun tb -> tb.IsChecked <- new Nullable<bool>(prop) )
                    | IsThreeState  prop -> ToggleButtonUpdate ( fun tb -> tb.IsThreeState <- prop)

            type ButtonBaseProperty with
                member x.PropertyUpdate() =
                    match x with
                    | ToggleButtonProperty tbp -> tbp.PropertyUpdate()

            type SingleContentProperty with
                member x.PropertyUpdate() =
                    match x with
                    | ButtonBaseProperty bbp  -> bbp.PropertyUpdate()
                    | Content            prop -> SingleContentUpdate( fun sc -> sc.Content <- prop)

            type ComboBoxProperty with
                member x.PropertyUpdate() =
                    match x with
                    | IsEditable            prop -> ComboBoxUpdate( fun cb -> cb.IsEditable <- prop)
                    | ComboBoxProperty.Text prop -> ComboBoxUpdate( fun cb -> cb.Text <- prop)

            type SelectableProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | ComboBoxProperty        cbp  -> cbp.PropertyUpdate()
                    | SelectedItem            prop -> SelectableUpdate( fun s -> s.SelectedItem <- prop)

            type CollectionContentProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | SelectableProperty        sp   -> sp.PropertyUpdate()
                    | IsTextSearchEnabled       prop -> CollectionContentUpdate( fun cc -> cc.IsTextSearchEnabled <- prop)
                    | IsTextSearchCaseSensitive prop -> CollectionContentUpdate( fun cc -> cc.IsTextSearchCaseSensitive <- prop)

            type ControlProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | RangeProperty             rp  -> rp.PropertyUpdate()
                    | TextBoxProperty           tbp -> tbp.PropertyUpdate()
                    | SingleContentProperty     scp -> scp.PropertyUpdate()
                    | CollectionContentProperty ccp -> ccp.PropertyUpdate()
        
            type StackPanelProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | CanHorizontallyScroll             prop -> StackPanelUpdate( fun sp -> sp.CanHorizontallyScroll <- prop)
                    | CanVerticallyScroll               prop -> StackPanelUpdate( fun sp -> sp.CanVerticallyScroll <- prop)
                    | StackPanelProperty.Orientation    prop -> StackPanelUpdate( fun sp -> sp.Orientation <- prop)

            type GridProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | ColumnDefinitions   prop  -> GridUpdate (fun gr -> prop |> List.iter(fun c -> gr.ColumnDefinitions.Add(c.Convert())) ) 
                    | RowDefinitions      prop  -> GridUpdate (fun gr -> prop |> List.iter(fun r -> gr.RowDefinitions.Add(r.Convert())) ) 

            type PanelProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | GridProperty          gp  -> gp.PropertyUpdate()
                    | StackPanelProperty    spp -> spp.PropertyUpdate()

        
            type TextBlockProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | Text          prop -> TextBlockUpdate( fun tb -> tb.Text <- prop )
                    | TextAlignment prop -> TextBlockUpdate( fun tb -> tb.TextAlignment <- prop )
                    | TextTrimming  prop -> TextBlockUpdate( fun tb -> tb.TextTrimming <- prop)
                    | TextWrapping  prop -> TextBlockUpdate( fun tb -> tb.TextWrapping <- prop)

            type LineProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | LineCoordinate prop -> 
                        let update (l:Line) =
                            l.X1 <- prop.X1
                            l.Y1 <- prop.Y1
                            l.X2 <- prop.X2
                            l.Y2 <- prop.Y2
                        LineUpdate update 

            type PolyLineProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | Points prop -> PolyLineUpdate ( fun pl -> prop |> List.iter(fun p -> pl.Points.Add(new Point(p.X,p.Y))) )

            type RectangleProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | Radius prop -> RectangeUpdate( fun r -> r.RadiusX <- prop.X ; r.RadiusY <- prop.Y )

            type ShapeProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | Fill              prop    -> ShapeUpdate( fun s -> s.Fill <- new SolidColorBrush(prop))
                    | Stroke            prop    -> ShapeUpdate( fun s -> s.Stroke <- new SolidColorBrush(prop))
                    | StrokeThickness   prop    -> ShapeUpdate( fun s -> s.StrokeThickness <- prop)
                    | LineProperty      lp      -> lp.PropertyUpdate() 
                    | PolyLineProperty  plp     -> plp.PropertyUpdate()
                    | RectangleProperty rp      -> rp.PropertyUpdate() 


            type FEProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | ControlProperty   cp  -> cp.PropertyUpdate()
                    | PanelProperty     pp  -> pp.PropertyUpdate()
                    | TextBlockProperty tbp -> tbp.PropertyUpdate()
                    | ShapeProperty     sp  -> sp.PropertyUpdate()

            type WindowProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | WindowStyle         prop  -> WindowUpdate (fun w -> w.WindowStyle <- prop) 
                    | WindowState         prop  -> WindowUpdate (fun w -> w.WindowState <- prop) 
                    | Title               prop  -> WindowUpdate (fun w -> w.Title <- prop) 
                    | ResizeMode          prop  -> WindowUpdate (fun w -> w.ResizeMode <- prop) 
                    | AllowsTransparency  prop  -> WindowUpdate (fun w -> w.AllowsTransparency <- prop) 
        
            type VProperty with
                member x.PropertyUpdate() =
                    match x with 
                    | VStyle            vs  -> vs.PropertyUpdate()
                    | WindowProperty    wp  -> wp.PropertyUpdate()
                    | FEProperty        fep -> fep.PropertyUpdate() 


       
            let updateProperties properties (uiElement : UIElement) =
                let (VProperties vprops) = properties
                vprops
                |> List.iter( fun (vprop,_) ->
                    match vprop.PropertyUpdate() with
                    | UIElementUpdate           update -> update (uiElement)
                    | FrameworkElementUpdate    update -> update (uiElement :?> FrameworkElement)
                    | ControlUpdate             update -> update (uiElement :?> Control)
                    | PanelUpdate               update -> update (uiElement :?> Panel)
                    | RangeBaseUpdate           update -> update (uiElement :?> RangeBase)
                    | ProgressBarUpdate         update -> update (uiElement :?> ProgressBar)
                    | SliderUpdate              update -> update (uiElement :?> Slider)
                    | TextBoxUpdate             update -> update (uiElement :?> TextBox)
                    | ToggleButtonUpdate        update -> update (uiElement :?> ToggleButton)
                    | ButtonBaseUpdate          update -> update (uiElement :?> ButtonBase)
                    | SingleContentUpdate       update -> update (uiElement :?> ContentControl)
                    | ComboBoxUpdate            update -> update (uiElement :?> ComboBox)
                    | SelectableUpdate          update -> update (uiElement :?> Selector)
                    | CollectionContentUpdate   update -> update (uiElement :?> ItemsControl)
                    | StackPanelUpdate          update -> update (uiElement :?> StackPanel)
                    | TextBlockUpdate           update -> update (uiElement :?> TextBlock)
                    | GridUpdate                update -> update (uiElement :?> Grid)
                    | WindowUpdate              update -> update (uiElement :?> Window)
                    | ShapeUpdate               update -> update (uiElement :?> Shape)
                    | LineUpdate                update -> update (uiElement :?> Line)
                    | PolyLineUpdate            update -> update (uiElement :?> Polyline)
                    | RectangeUpdate            update -> update (uiElement :?> Rectangle)
                    | BorderUpdate              update -> update (uiElement :?> Border)
                  )


        (*** ********************************** ***)
        (***         EventHandling Module       ***)
        (*** ********************************** ***)
        /// Module which contains extension to handle
        /// WPF event mechanism : Adding handler and removing them to ensure :
        /// disposing of the WPF objects, correct behaviour being described is shown...
        module EventHandling =
            open VPropertyUpdate
            open VirtualEvent
            open System.Windows.Shapes



            type TextBoxEvent with           
                member x.EventAdd() =     
                    match x with
                    | TextChanged handler -> TextBoxUpdate( fun tb -> tb.TextChanged.AddHandler handler)
                member x.EventDispose() =     
                    match x with
                    | TextChanged handler -> TextBoxUpdate( fun tb -> tb.TextChanged.RemoveHandler handler)
                    
            type ToggleButtonEvent with           
                member x.EventAdd() =     
                    match x with
                    | Checked       handler -> ToggleButtonUpdate( fun tb -> tb.Checked.AddHandler handler)
                    | Unchecked     handler -> ToggleButtonUpdate( fun tb -> tb.Unchecked.AddHandler handler)
                    | Indeterminate handler -> ToggleButtonUpdate( fun tb -> tb.Indeterminate.AddHandler handler)
                member x.EventDispose() =     
                    match x with
                    | Checked       handler -> ToggleButtonUpdate( fun tb -> tb.Checked.RemoveHandler handler)
                    | Unchecked     handler -> ToggleButtonUpdate( fun tb -> tb.Unchecked.RemoveHandler handler)
                    | Indeterminate handler -> ToggleButtonUpdate( fun tb -> tb.Indeterminate.RemoveHandler handler)

            type ButtonBaseEvent with           
                member x.EventAdd() =     
                    match x with
                    | Click             handler -> ButtonBaseUpdate( fun bb -> bb.Click.AddHandler handler)
                    | ToggleButtonEvent tb      -> tb.EventAdd()
                member x.EventDispose() =     
                    match x with
                    | Click             handler -> ButtonBaseUpdate( fun bb -> bb.Click.RemoveHandler handler)
                    | ToggleButtonEvent tb      -> tb.EventDispose()

            type SingleContentEvent with           
                member x.EventAdd() =     
                    match x with
                    | ButtonBaseEvent bb -> bb.EventAdd()
                member x.EventDispose() =     
                    match x with
                    | ButtonBaseEvent bb -> bb.EventDispose()

            type SelectableEvent with           
                member x.EventAdd() =     
                    match x with
                    | SelectionChanged handler -> SelectableUpdate( fun s -> s.SelectionChanged.AddHandler handler)
                member x.EventDispose() =     
                    match x with
                    | SelectionChanged handler -> SelectableUpdate( fun s -> s.SelectionChanged.RemoveHandler handler)

            type CollectionContentEvent with           
                member x.EventAdd() =     
                    match x with
                    | SelectableEvent s -> s.EventAdd()
                member x.EventDispose() =     
                    match x with
                    | SelectableEvent s -> s.EventDispose()

            type RangeBaseEvent with           
                member x.EventAdd() =     
                    match x with
                    | ValueChanged handler -> RangeBaseUpdate( fun rb -> rb.ValueChanged.AddHandler handler)
                member x.EventDispose() =     
                    match x with
                    | ValueChanged handler -> RangeBaseUpdate( fun rb -> rb.ValueChanged.RemoveHandler handler)

            type ControlEvent with           
                member x.EventAdd() =     
                    match x with
                    | RangeBaseEvent            rb -> rb.EventAdd()
                    | TextBoxEvent              tb -> tb.EventAdd()
                    | SingleContentEvent        sc -> sc.EventAdd()
                    | CollectionContentEvent    cc -> cc.EventAdd()
                member x.EventDispose() =     
                    match x with
                    | RangeBaseEvent            rb -> rb.EventDispose()
                    | TextBoxEvent              tb -> tb.EventDispose()
                    | SingleContentEvent        sc -> sc.EventDispose()
                    | CollectionContentEvent    cc -> cc.EventDispose()
        
            type FEEvent with           
                member x.EventAdd() =     
                    match x with
                    | ControlEvent c -> c.EventAdd()
                member x.EventDispose() =     
                    match x with
                    | ControlEvent c -> c.EventDispose()

            type WindowEvent with           
                member x.EventAdd() =     
                    match x with
                    | Activated     handler -> WindowUpdate( fun w -> w.Activated.AddHandler handler)     
                    | Closed        handler -> WindowUpdate( fun w -> w.Closed.AddHandler handler)     
                    | Closing       handler -> WindowUpdate( fun w -> w.Closing.AddHandler handler)
                    | Deactivated   handler -> WindowUpdate( fun w -> w.Deactivated.AddHandler handler)
                    | Loaded        handler -> WindowUpdate( fun w -> w.Loaded.AddHandler handler)
                member x.EventDispose() =     
                    match x with
                    | Activated     handler -> WindowUpdate( fun w -> w.Activated.RemoveHandler handler)     
                    | Closed        handler -> WindowUpdate( fun w -> w.Closed.RemoveHandler handler)     
                    | Closing       handler -> WindowUpdate( fun w -> w.Closing.RemoveHandler handler)
                    | Deactivated   handler -> WindowUpdate( fun w -> w.Deactivated.RemoveHandler handler)
                    | Loaded        handler -> WindowUpdate( fun w -> w.Loaded.RemoveHandler handler)

            type VEvent with           
                member x.EventAdd() =     
                    match x with
                    | NoEvent        -> failwith "Some failure to handle"
                    | WindowEvent w  -> w.EventAdd() 
                    | FEEvent     fe -> fe.EventAdd()
                member x.EventDispose() =     
                    match x with
                    | NoEvent        -> failwith "Some failure to handle"
                    | WindowEvent w  -> w.EventDispose() 
                    | FEEvent     fe -> fe.EventDispose()


            let private handleEvents (action:VEvent -> WPFObjectUpdate) events (uiElement : UIElement) =              
                let (VEvents vevents) = events
                vevents
                |> List.iter( fun (vevent,_) ->
                    match action vevent with
                    | UIElementUpdate           update -> update (uiElement)
                    | FrameworkElementUpdate    update -> update (uiElement :?> FrameworkElement)
                    | ControlUpdate             update -> update (uiElement :?> Control)
                    | PanelUpdate               update -> update (uiElement :?> Panel)
                    | RangeBaseUpdate           update -> update (uiElement :?> RangeBase)
                    | ProgressBarUpdate         update -> update (uiElement :?> ProgressBar)
                    | SliderUpdate              update -> update (uiElement :?> Slider)
                    | TextBoxUpdate             update -> update (uiElement :?> TextBox)
                    | ToggleButtonUpdate        update -> update (uiElement :?> ToggleButton)
                    | ButtonBaseUpdate          update -> update (uiElement :?> ButtonBase)
                    | SingleContentUpdate       update -> update (uiElement :?> ContentControl)
                    | ComboBoxUpdate            update -> update (uiElement :?> ComboBox)
                    | SelectableUpdate          update -> update (uiElement :?> Selector)
                    | CollectionContentUpdate   update -> update (uiElement :?> ItemsControl)
                    | StackPanelUpdate          update -> update (uiElement :?> StackPanel)
                    | TextBlockUpdate           update -> update (uiElement :?> TextBlock)
                    | GridUpdate                update -> update (uiElement :?> Grid)
                    | WindowUpdate              update -> update (uiElement :?> Window)
                    | ShapeUpdate               update -> update (uiElement :?> Shape)
                    | LineUpdate                update -> update (uiElement :?> Line)
                    | PolyLineUpdate            update -> update (uiElement :?> Polyline)
                    | RectangeUpdate            update -> update (uiElement :?> Rectangle)
                    | BorderUpdate              update -> update (uiElement :?> Border)
                    )

            let internal addHandlerEvents events (uiElement : UIElement) = handleEvents (fun vevent -> vevent.EventAdd()) events uiElement
            
            let internal disposeHandlerEvents events (uiElement: UIElement) = handleEvents (fun vevent -> vevent.EventDispose()) events uiElement



        (*** ********************************** ***)
        (***         VCreation Module           ***)
        (*** ********************************** ***)
        /// Module which contains extension to create from scratch
        /// WPF tree object from its virtual representation 
        module VCreation = 
            open EventHandling
            open VPropertyUpdate
            open System.Windows.Shapes

            type TaggedContainer with
                member x.Create() =
                    match x with
                    | Grid        -> new Grid()       :> UIElement
                    | StackPanel  -> new StackPanel() :> UIElement
                    | Canvas      -> new Canvas()     :> UIElement
            
            type TaggedControl  with
                member x.Create() =
                    match x with
                    | Button      -> new Button()       :> UIElement
                    | CheckBox    -> new CheckBox()     :> UIElement
                    | ToggleButton-> new ToggleButton() :> UIElement
                    | RadioButton -> new RadioButton()  :> UIElement
                    | ProgressBar -> new ProgressBar()  :> UIElement
                    | TextBox     -> new TextBox()      :> UIElement
                    | Slider      -> new Slider()       :> UIElement
                    | Ellipse     -> new Ellipse()      :> UIElement
                    | Line        -> new Line()         :> UIElement
                    | Rectangle   -> new Rectangle()    :> UIElement
                    | Polyline    -> new Polyline()     :> UIElement
        
            type TaggedBorder with
                member x.Create() =
                    match x with
                    | Border      -> new Border()       :> UIElement

            type TaggedTextBlock with
                member x.Create() =
                    match x with
                    | TextBlock -> new TextBlock() :> UIElement
        
            type TaggedSingle with
                member x.Create() =
                    match x with
                    | TaggedControl     tc  -> tc.Create()
                    | TaggedTextBlock   ttb -> ttb.Create()
                    | TaggedBorder      tb  -> tb.Create()

            type TaggedItems with
                member x.Create() =
                    match x with
                    | ComboBox    -> new ComboBox() :> UIElement
           
            type Tag<'Msg> with
                member x.Create() =
                    match x with
                    | NodeContainer (nodeContainer, trees)  -> 
                        let append (uiElement:UIElement) (uiElement2:UIElement) = (uiElement :?> Panel).Children.Add(uiElement2) |> ignore
                        nodeContainer.Create(),trees,append
                    | NodeItems     (nodeItems, items)      -> 
                        let append (uiElement:UIElement) (uiElement2:UIElement) = (uiElement :?> ItemsControl).Items.Add(uiElement2) |> ignore                        
                        nodeItems.Create(),items,append      
                    | NodeSingle    (nodeSingle)           -> nodeSingle.Create(), [], (fun _ _ -> ())

            let rec private addToUIElement (append : UIElement -> UIElement -> unit) (uiElement : UIElement) (trees : WPFTree<'Msg> list) =
                for tree in trees do
                    let (WPFTree nodeElement) = tree
                    let (uiElement2,subTrees,appendChild) = nodeElement.Tag.Create()

                    updateProperties (nodeElement.Properties) uiElement2
                    addHandlerEvents (nodeElement.Events) uiElement2
                    if not trees.IsEmpty then
                        append uiElement uiElement2
                        addToUIElement appendChild uiElement2 subTrees

            type WPFTree<'Msg> with
                member x.Create() =
                    let (WPFTree nodeElement) = x      
                    let (uiElement,trees,append) = nodeElement.Tag.Create()
                    updateProperties (nodeElement.Properties) uiElement 
                    addHandlerEvents (nodeElement.Events) uiElement 
                    addToUIElement append uiElement trees
                    uiElement
                

            type WPFWindow<'Msg> with
                member x.Build() =
                    let tree = x.Tree
                    //let (ViewWindow (nodeElement, tree)) = x
                    let window = new Window() //nodeElement.Tag.Create() :?> Window
                    let _ = 
                        let (VProperties vprops) = x.Properties
                        vprops
                        |> List.iter( fun (vprop,_) ->
                            match vprop.PropertyUpdate() with
                            | WindowUpdate              windowUp        -> windowUp window
                            | UIElementUpdate           uiElementUp     -> uiElementUp window
                            | FrameworkElementUpdate    frameworkElemUp -> frameworkElemUp window
                            | ControlUpdate             controlUp       -> controlUp window
                            | _                                         -> failwith "Code mistake"
                           )
                        let (VEvents vevents) = x.Events
                        vevents
                        |> List.iter( fun (vevent,_) ->
                            match vevent.EventAdd() with
                            | WindowUpdate              windowUp        -> windowUp window
                            | _                                         -> failwith "Code mistake"
                           )
                    let (WPFTree nodeElement) = tree
                    let (uiElement,trees,append) = nodeElement.Tag.Create()

                    updateProperties (nodeElement.Properties) uiElement
                    addHandlerEvents (nodeElement.Events) uiElement

                    window.Content <- uiElement
                    addToUIElement append uiElement trees
                    window



        (*** ********************************** ***)
        (***         VirtualConvert Module      ***)
        (*** ********************************** ***)
        /// Module which contains extension to convert the  from scratch
        /// virtual tree to another virtual tree by updating the event handler
        module VirtualConvert =
            open VirtualEvent
            open EventHandling

            type WPFEvent<'Msg> with
                member x.VirtualConvert(dispatch) : VEvent =
                    let buildHandler getMsg handlerLambda =
                        ( fun _ args -> 
                            let msg = getMsg args
                            dispatch msg )
                        |> handlerLambda
                    match x with
                    | WPFClick              (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Click          |> ButtonBaseEvent |> SingleContentEvent |> ControlEvent |> FEEvent
                    | WPFTextChanged        (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> TextChanged    |> TextBoxEvent |> ControlEvent |> FEEvent
                    | WPFActivated          (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Activated      |> WindowEvent
                    | WPFClosed             (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Closed         |> WindowEvent
                    | WPFClosing            (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Closing        |> WindowEvent
                    | WPFDeactivated        (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Deactivated    |> WindowEvent
                    | WPFLoaded             (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Loaded         |> WindowEvent
                    | WPFChecked            (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Checked        |> ToggleButtonEvent |> ButtonBaseEvent |> SingleContentEvent |> ControlEvent |> FEEvent
                    | WPFUnchecked          (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Unchecked      |> ToggleButtonEvent |> ButtonBaseEvent |> SingleContentEvent |> ControlEvent |> FEEvent
                    | WPFIndeterminate      (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> Indeterminate  |> ToggleButtonEvent |> ButtonBaseEvent |> SingleContentEvent |> ControlEvent |> FEEvent
                    | WPFValueChanged       (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> ValueChanged   |> RangeBaseEvent |> ControlEvent |> FEEvent
                    | WPFSelectionChanged   (getMsg,handlerLambda)  -> buildHandler getMsg handlerLambda |> SelectionChanged |> SelectableEvent |> CollectionContentEvent |> ControlEvent |> FEEvent

            type WPFEvents<'Msg> with 
                member x.VirtualConvert(dispatch) : VEvents =
                    let (WPFEvents wpfEventList) = x
                    wpfEventList 
                    |> List.map(fun (wpfEvent,index) -> (wpfEvent.VirtualConvert(dispatch),index))
                    |> VEvents

            type WPFNodeElement<'Msg> with
                member x.VirtualConvert(dispatch) : WPFNodeElement<'Msg> =
                    { x with Events = x.WPFEvents.VirtualConvert(dispatch) }

            type WPFTree<'Msg> with
                member x.VirtualConvert(dispatch) : WPFTree<'Msg> =
                    let rec aux (wpfTree:WPFTree<'Msg>) =
                        let (WPFTree wpfNodeElement) = wpfTree
                        let vNodeElement = wpfNodeElement.VirtualConvert(dispatch)
                        let tag =
                            match vNodeElement.Tag with
                            | NodeContainer (container, wpfTrees) -> 
                                let vTrees = wpfTrees |> List.map(fun wpfTree -> aux wpfTree )
                                NodeContainer (container, vTrees)
                            | NodeItems     (items, wpfTrees) -> 
                                let vTrees = wpfTrees |> List.map(fun wpfTree -> aux wpfTree )
                                NodeItems     (items, vTrees)
                            | NodeSingle   nodeSingle         ->
                                NodeSingle   nodeSingle          

                        WPFTree { vNodeElement  with Tag = tag }
                    aux x
         

            type WPFWindow<'Msg> with
                member x.VirtualConvert(dispatch) : WPFWindow<'Msg> =            
                    { x with 
                          Events = x.WPFEvents.VirtualConvert(dispatch)  
                          Tree = x.Tree.VirtualConvert(dispatch) }





    module VDom =
        open System.Windows.Controls
        open VDomTypes
        open VDomExt.EventHandling
        open VDomExt.VPropertyUpdate
        open VDomExt.VCreation
        open VirtualEvent
        open VirtualProperty
        open VirtualPropertyDefaultValues
    // TODO : 
    // X - Define differenciation of VDOM
    // X - Tag VDOM such that when traversing it in parallel to real DOM
    //    we can know which part of the real DOM we need to update
    // X - Traverse real DOM in parallel to VDOM
    // TODO :
    //  -   The implementation is super basic with super bad perf I would suspect, 
    //      Think of a way to make/implement optimized type-safe algorithms for 
    //      doing tree diffing => update list + updating UI Elements
    //  -   Have a more well defined Domain Model to ensure the safety of the implementation


        // Locate the node as a list of children position
        type NodeLoc = NodeLoc of int list
    

        type AddEvents      = AddEvents of VEvents
        type RemoveEvents   = RemoveEvents of VEvents

        type Update<'Msg> =
            | UpEvents      of NodeLoc * RemoveEvents * AddEvents
            | UpProperties  of NodeLoc * VProperties
            | UpNode        of NodeLoc * WPFTree<'Msg>
            | AddNode       of NodeLoc * WPFTree<'Msg>
            | RemoveNode    of NodeLoc 



        let private eventsDifferences (VEvents evOld) (VEvents evNew) (nodeLoc : NodeLoc) =
            let rec aux (evOld:(VEvent*int) list) (evNew:(VEvent*int) list) (removeEvents : (VEvent*int) list) (addEvents : (VEvent*int) list) =
                match evOld,evNew with
                | (hdOld,indOld)::tlOld , (hdNew,indNew)::tlNew ->
                    if indOld > indNew then
                        aux tlOld evNew ((hdOld,indOld)::removeEvents) addEvents
                    elif indOld < indNew then
                        aux evOld tlNew removeEvents ((hdNew,indNew)::addEvents)
                    else
                        aux tlOld tlNew  ((hdOld,indOld)::removeEvents) ((hdNew,indNew)::addEvents)
                    
                | [] , (hd,ind)::tl -> aux [] tl removeEvents ((hd,ind)::addEvents)
                | (hd,ind)::tl , [] -> aux tl [] ((hd,ind)::removeEvents) addEvents
                | [] , [] -> (removeEvents,addEvents)
        
            match (aux evOld evNew [] []) with
            | [] , []           -> UpEvents (nodeLoc, RemoveEvents (VEvents []) , AddEvents (VEvents []) )
            | [] , addEvents    -> UpEvents (nodeLoc, RemoveEvents (VEvents []) , AddEvents (VEvents addEvents) )
            | remEvents , []    -> UpEvents (nodeLoc, RemoveEvents (VEvents remEvents) , AddEvents (VEvents []) )
            | remEvents , addEvents -> UpEvents (nodeLoc, RemoveEvents (VEvents remEvents) , AddEvents (VEvents addEvents) )


    
        /// finds the differences to do for 2 list of properties 
        let private propertiesDifferences (VProperties propOld) (VProperties propNew) (nodeLoc : NodeLoc) =
            // Because of the bind function called to generate the list, the order of the list is reversed
            // thus we inverse the behaviour for when comparing the index of the OLD property element and the index of the 
            // NEW property element.
            let rec aux (propOld:(VProperty*int) list) (propNew:(VProperty*int) list) (updates : (VProperty*int) list) =
                match propOld,propNew with
                | (hdOld,indOld)::tlOld , (hdNew,indNew)::tlNew ->
                    if indOld > indNew then
                        aux tlOld propNew ((hdOld.DefaultValue ,indOld)::updates)
                    elif indOld < indNew then
                        aux propOld tlNew ((hdNew,indNew)::updates)
                    else
                        if hdOld = hdNew then
                            aux tlOld tlNew updates
                        else
                            aux tlOld tlNew ((hdNew,indNew)::updates)
            
                | [] , (hd,ind)::tl -> aux [] tl ((hd,ind)::updates)
                | (hd,ind)::tl , [] -> aux tl [] ((hd.DefaultValue ,ind)::updates)
                | [] , [] -> updates
        
            match (aux propOld propNew []) with
            | []        -> UpProperties (nodeLoc, VProperties [])
            | vprops    -> UpProperties (nodeLoc, VProperties vprops)                


        let treeDiff (windowOld:WPFWindow<'Msg>) (windowNew:WPFWindow<'Msg>) =
            let nodeDiffs (nodeOld:WPFNodeElement<'Msg>) (nodeNew:WPFNodeElement<'Msg>) (NodeLoc nodeLoc) : (Update<'Msg> * Update<'Msg>) option =
                let tagsAreEqual =
                    match nodeOld.Tag,nodeNew.Tag with
                    | NodeContainer (oldContainer, _), NodeContainer (newContainer, _) when oldContainer = newContainer -> true
                    | NodeItems     (oldItems,_), NodeItems     (newItems,_) when oldItems = newItems -> true
                    | NodeSingle    oldNodeSingle, NodeSingle   newNodeSingle when oldNodeSingle = newNodeSingle -> true
                    | _,_ -> false
                
                if tagsAreEqual then
                    let upProps  = propertiesDifferences (nodeOld.Properties) (nodeNew.Properties) (NodeLoc nodeLoc)
                    let upEvents = eventsDifferences (nodeOld.Events) (nodeNew.Events) (NodeLoc nodeLoc)
                    Some (upEvents,upProps)
                else 
                    None
            
            
            let rec aux (oldTrees:WPFTree<'Msg> list) (newTrees:WPFTree<'Msg> list) (NodeLoc revList) (lineLoc:int) (updates: Update<'Msg> list) =
                match oldTrees,newTrees with
                | [] , []   -> updates
                | [] , _    -> 
                    let ups = 
                        newTrees
                        |> List.mapi(fun index sub -> 
                            let nodeLoc = (lineLoc + index)::revList |> List.rev
                            AddNode ((NodeLoc nodeLoc),sub) 
                            )
                        |> List.rev
                    ups@updates                    
                | _ , []    -> 
                    let ups = 
                        oldTrees
                        |> List.mapi(fun index _ -> 
                            let nodeLoc = (lineLoc + index)::revList |> List.rev
                            RemoveNode (NodeLoc nodeLoc) 
                            )
                    ups@updates                
                | hdOld::tlOld , hdNew::tlNew   ->
                    let nl = 
                        let nl = (lineLoc::revList) |> List.rev
                        nl 
                    let (WPFTree oldNode) = hdOld
                    let (WPFTree newNode) = hdNew
                    let diff = nodeDiffs oldNode newNode (NodeLoc nl)
                    let ups = 
                        match diff with
                        | None -> aux [] [] (NodeLoc (List.rev nl)) 0 ((UpNode((NodeLoc nl),hdNew))::updates)
                        | Some (upEvents,upProps) -> 
                            let oldSubTrees =
                                match oldNode.Tag with
                                | NodeContainer (_,subTrees) -> subTrees
                                | NodeItems (_,subTrees) -> subTrees
                                | NodeSingle _ -> []
                            let newSubTrees =
                                match newNode.Tag with
                                | NodeContainer (_,subTrees) -> subTrees
                                | NodeItems (_,subTrees) -> subTrees
                                | NodeSingle _ -> []

                            aux oldSubTrees newSubTrees (NodeLoc (List.rev nl)) 0 (upEvents::upProps::updates)
                    aux tlOld tlNew (NodeLoc revList) (lineLoc + 1) ups

            let upProps  = propertiesDifferences (windowOld.Properties) (windowNew.Properties) (NodeLoc [])
            let upEvents = eventsDifferences (windowOld.Events) (windowNew.Events) (NodeLoc [])

            let updates = aux [windowOld.Tree] [windowNew.Tree] (NodeLoc []) 0 []

            upEvents::upProps::(List.rev updates)

        
        let private getUIElement (window:Window) (nodeLoc:int list) = 
            let getNthChild (uiElement:UIElement) (n:int) =
                match uiElement with
                | :? Panel as panel -> panel.Children.Item(n)
                | :? ItemsControl as items -> items.Items.Item(n) :?> UIElement
                | _ -> failwith "Failure in Code"

            let rec aux (uiElement:UIElement) (nodeLoc:int list) =
                match nodeLoc with
                | [] -> uiElement
                | hd::tl ->
                    let uiElement = getNthChild uiElement hd
                    aux uiElement tl

            match nodeLoc with
            | [] -> window :> UIElement
            | _  -> aux (window.Content :?> UIElement) nodeLoc.Tail


        let private getUIElementAndDoAction (window:Window) (nodeLoc:int list) (action1:Panel -> unit) (action2:ItemsControl -> unit) =
            match (getUIElement window nodeLoc) with
            | :? Panel as panel -> action1 panel
            | :? ItemsControl as items -> action2 items
            | _ -> failwith "Failure in Code"




        let updateWindow (window:Window) (updates : Update<'Msg> list) = 
            let rec aux (updates : Update<'Msg> list) =
                match updates with
                | [] -> ()
                | update::tl ->
                    match update with
                    | UpEvents      ((NodeLoc nodeLoc), RemoveEvents removeEvents, AddEvents addEvents) ->
                        let uiElement = getUIElement window nodeLoc
                        disposeHandlerEvents removeEvents uiElement
                        addHandlerEvents addEvents uiElement
                    | UpProperties  ((NodeLoc nodeLoc),vprops) ->
                        let uiElement = getUIElement window nodeLoc
                        updateProperties vprops uiElement
                    | UpNode        ((NodeLoc nodeLoc),tree) ->
                        let revList = List.rev nodeLoc
                        let index = revList.Head

                        let action1 (panel:Panel) =
                            panel.Children.RemoveAt(index)
                            panel.Children.Insert(index,tree.Create())
                        
                        let action2 (itemsControl:ItemsControl) =
                            itemsControl.Items.RemoveAt(index)
                            itemsControl.Items.Insert(index,tree.Create())
                         
                        getUIElementAndDoAction window (revList.Tail |> List.rev) action1 action2
                    | AddNode       ((NodeLoc nodeLoc),tree) ->
                        let revList = List.rev nodeLoc
                        let index = revList.Head

                        let action1 (panel:Panel) = panel.Children.Insert(index,tree.Create())                                                
                        let action2 (itemsControl:ItemsControl) = itemsControl.Items.Insert(index,tree.Create()) 

                        getUIElementAndDoAction window (revList.Tail |> List.rev) action1 action2
                    | RemoveNode    (NodeLoc nodeLoc) ->
                        let revList = List.rev nodeLoc
                        let index = revList.Head

                        let action1 (panel:Panel) = panel.Children.RemoveAt(index)                                      
                        let action2 (itemsControl:ItemsControl) = itemsControl.Items.RemoveAt(index)

                        getUIElementAndDoAction window (revList.Tail |> List.rev) action1 action2
                
                    aux tl
            aux updates