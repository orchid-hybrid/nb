datatype token =
  BoldQuote | ItalicQuote
| OpenSquareBrackets | CloseSquareBrackets | LinkBar
| Bullet | NumberedBullet
| Heading1 | Heading2
| StringToken of string

datatype machine_state =
  UsualState
| LinkState
| BulletState
| HeadingState

val show_token : show token =
    mkShow (fn i =>
               case i of
                   BoldQuote => "''"
                 | ItalicQuote => "'''"
                 | OpenSquareBrackets => "[["
                 | CloseSquareBrackets => "]]"
                 | LinkBar => "|"
                 | Bullet => "*"
                 | NumberedBullet => "#"
                 | Heading1 => "=="
                 | Heading2 => "==="
                 | StringToken s => String.append "\"" (String.append s "\""))

fun tokenize s =
    let
        val len = String.length s

        fun safesub i =
            if i >= len then
                None
            else
                Some (String.sub s i)
        
        fun machine_start i =
            case String.sub s i of
                #"*" => (case safesub (i+1) of
                             Some #" " => Some (2, Bullet, BulletState)
                           | _ => Some (1, Bullet, BulletState))
              | #"#" => (case safesub (i+1) of
                             Some #" " => Some (2, NumberedBullet, BulletState)
                           | _ => Some (1, NumberedBullet, BulletState))
              | #"=" => (case safesub (i+1) of
                             Some #"=" => (case safesub (i+2) of
                                               Some #"=" => Some (3, Heading2, HeadingState)
                                             | _ => Some (2, Heading1, HeadingState))
                           | _ => None)
              | _ => machine_usual i
        and machine_usual i =
            case String.sub s i of
                #"'" => (case safesub (i+1) of
                             Some #"'" => (case safesub (i+2) of
                                               Some #"'" => Some (3, ItalicQuote, UsualState)
                                             | _ => Some (2, BoldQuote, UsualState))
                           | _ => None)
              | #"[" => (case safesub (i+1) of
                             Some #"[" => Some (2, OpenSquareBrackets, LinkState)
                           | _ => None)
              | _ => None
        and machine_link i =
            case String.sub s i of
                #"]" => (case safesub (i+1) of
                             Some #"]" => Some (2, CloseSquareBrackets, UsualState)
                           | _ => None)
              | #"|" => Some (1, LinkBar, LinkState)
              | _ => None
        and machine_bullet i =
            None
        and machine_heading i =
            case String.sub s i of
                #"=" => (case safesub (i+1) of
                             Some #"=" => (case safesub (i+2) of
                                               Some #"=" => Some (3, Heading2, HeadingState)
                                             | _ => Some (2, Heading1, HeadingState))
                           | _ => None)
              | _ => None
        fun machine i state =
            case state of
                UsualState => if eq i 0 then
                                  machine_start i
                              else
                                  machine_usual i
              | LinkState => machine_link i
              | BulletState => machine_bullet i
              | HeadingState => machine_heading i

        fun tok start pointer state =
            if pointer >= len then
                if eq start pointer then
                    []
                else
                    StringToken (String.substring s {Start = start, Len = pointer - start}) :: []
            else
                case machine pointer state of
                    Some (adv,t,st) => if eq start pointer then
                                           t :: tok (pointer + adv) (pointer + adv) st
                                       else
                                           StringToken (String.substring s {Start = start, Len = pointer - start}) :: t :: tok (pointer + adv) (pointer + adv) st
                  | None => tok start (pointer + 1) state
    in
        tok 0 0 UsualState
    end

datatype styles =
| Bold
| Italic
| Error

datatype formatted =
| Nothing
| String of string
| Append of formatted * formatted
| Styled of styles * formatted
| Link of string * option string

datatype tree =
  BulletPoints of bool * list string
| Heading of int * string
| Paragraph of formatted
| LineBreak

fun lines (s : string) =
    let
        fun split s ch =
            case String.index s ch of
                None => None
              | Some i => Some (String.substring s {Start = 0, Len = i-1},
                                String.suffix s (i + 1))
    in
        case split s #"\n" of
            None => s :: []
          | Some (s1, s2) => s1 :: lines s2
    end

fun parse line_blank l =
    case l of
        (Bullet :: (StringToken s) :: []) :: ls => collect_bullets (s :: []) ls
      | (Heading1 :: (StringToken s) :: Heading1 :: []) :: ls => Heading (1, s) :: parse False ls
      | (Heading2 :: (StringToken s) :: Heading2 :: []) :: ls => Heading (1, s) :: parse False ls
      | l :: ls =>
        let
            val p = parse_paragraph l
        in
            (case p of
                 Nothing => if line_blank then
                                LineBreak :: parse False ls
                            else
                                parse True ls
               | _ => Paragraph p :: parse False ls)
        end
      | [] => []
and parse_paragraph l =
    let
        fun join_paragraphs p q =
            case (p,q) of
                (Nothing, q) => q
              | (p, Nothing) => p
              | (p, q) => Append (p,q)

        fun loop tag acc l =
            case l of
                [] => acc
              | (StringToken s) :: ls => loop tag (join_paragraphs acc (String s)) ls
              | BoldQuote :: ls => (case tag of
                                        None => join_paragraphs acc (loop (Some Bold) Nothing ls)
                                      | Some Bold => join_paragraphs (Styled (Bold,acc)) (loop None Nothing ls)
                                      | _ => join_paragraphs (Styled (Error,acc)) (loop None Nothing ls))
              | ItalicQuote :: ls => (case tag of
                                          None => join_paragraphs acc (loop (Some Italic) Nothing ls)
                                        | Some Italic => join_paragraphs (Styled (Italic,acc)) (loop None Nothing ls)
                                        | _ => join_paragraphs (Styled (Error,acc)) (loop None Nothing ls))
              | OpenSquareBrackets :: (StringToken s) :: CloseSquareBrackets :: ls => loop tag (join_paragraphs acc (Link (s, None))) ls
              | OpenSquareBrackets :: (StringToken s) :: LinkBar :: (StringToken n) :: CloseSquareBrackets :: ls => loop tag (join_paragraphs acc (Link (s, Some n))) ls
              | t :: ls => join_paragraphs (Styled (Error,String (show t))) (loop tag acc ls)
    in
        loop None Nothing l
    end

and collect_bullets acc l =
    case l of
        (Bullet :: (StringToken s) :: []) :: ls => collect_bullets (s :: acc) ls
      | _ => BulletPoints (False, List.rev acc) :: parse False l

fun render link e =
    case e of
        (BulletPoints (b, bs)) :: es => <xml>{render_bullets b bs}{render link es}</xml>
      | (Heading (i, s)) :: es => <xml><h1>{[s]}</h1>{render link es}</xml>
      | (Paragraph p') :: (Paragraph q') :: es => render link (Paragraph (Append (p', q')) :: es)
      | (Paragraph p') :: es => <xml><p>{render_paragraph link p'}</p>{render link es}</xml>
      | LineBreak :: es => <xml>{render link es}</xml>
      | [] => <xml/>
and render_paragraph link p =
    case p of
        Nothing => <xml/>
      | String s => <xml>{[s]}</xml>
      | Styled (Bold,p) => <xml><b>{render_paragraph link p}</b></xml>
      | Styled (Italic,p) => <xml><i>{render_paragraph link p}</i></xml>
      | Styled (Error,p) => <xml><span style={STYLE "color:red"}>{render_paragraph link p}</span></xml>
      | Append (p,q) => <xml>{render_paragraph link p}{render_paragraph link q}</xml>
      | Link (s, None) => <xml><a link={link s}>{[s]}</a></xml>
      | Link (s, Some n) => <xml><a link={link s}>{[n]}</a></xml>
and render_bullets b bs =
    let
        fun loop bs =
            case bs of
                b :: bs => <xml><li>{[b]}</li>{loop bs}</xml>
              | [] => <xml/>
    in
        <xml><ul>{loop bs}</ul></xml>
    end

fun markup link s = render link (parse False (List.mp tokenize (lines s)))
