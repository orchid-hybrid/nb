(*
 * SQL tables
 *)

sequence s

table metatbl : {
      Id : int,
      Title : string,
      Revisions : int
} PRIMARY KEY Id

table notetbl : {
      Id : int,
      Revision : int,
      Content : string,
      Comment : string,
      Time : time
} PRIMARY KEY (Id, Revision)

table tagstabl : {
      Id : int,
      Tag : string
} CONSTRAINT Id FOREIGN KEY Id REFERENCES metatbl(Id)

                 
(*
 * SQL query helpers
 *)

type result = { Revision : int, Content : string }

fun latest_revision title : transaction (option result) =
    mOpt <- oneOrNoRows
                (SELECT metatbl.Id, metatbl.Revisions
                 FROM metatbl
                 WHERE metatbl.Title={[title]});
    case mOpt of
        None => return None
      | Some m =>
        rOpt <- oneOrNoRows
                    (SELECT notetbl.Content
                     FROM notetbl
                     WHERE notetbl.Id={[m.Metatbl.Id]}
                       AND notetbl.Revision={[m.Metatbl.Revisions]});
        case rOpt of
            None => return None
          | Some r => return (Some ({ Revision = m.Metatbl.Revisions,
                                      Content = r.Notetbl.Content }))

fun specific_revision (title : string) (rev : int) =
    mOpt <- oneOrNoRows
                (SELECT metatbl.Id
                 FROM metatbl
                 WHERE metatbl.Title={[title]});
    case mOpt of
        None => return None
      | Some m =>
        rOpt <- oneOrNoRows
                    (SELECT notetbl.Content
                     FROM notetbl
                     WHERE notetbl.Id={[m.Metatbl.Id]}
                       AND notetbl.Revision={[rev]});
        case rOpt of
            None => return None
          | Some r => return (Some ({ Content = r.Notetbl.Content }))

fun history_of_revisions title maker =
    mOpt <- oneOrNoRows
                (SELECT metatbl.Id, metatbl.Revisions
                 FROM metatbl
                 WHERE metatbl.Title={[title]});
    case mOpt of
        None => return None
      | Some m =>
        hist <- queryX
                    (SELECT notetbl.Id, notetbl.Revision, notetbl.Time
                     FROM notetbl
                     WHERE notetbl.Id={[m.Metatbl.Id]}
                     ORDER BY notetbl.Time DESC)
                    maker;
        return (Some hist)

        
(*
 * page templates/helpers
 *)

fun makeButtons l =
    let
        fun loop l =
            case l of
                [] => <xml></xml>
              | x :: [] => x
              | x :: xs => <xml>{x} / {loop xs}</xml>
    in
        <xml>
          [
          {loop l}
          ]
        </xml>
    end

fun noteTemplate buttons title bodey =
    return <xml>
      <head>
        <link rel="stylesheet" type="text/css" href="/scripts/style.css"/>
      </head>
      <body>
        <div>
          <div style={STYLE "float: left;"}>
            {makeButtons buttons}
          </div>
          <div style={STYLE "float: right;"}>
            {makeButtons (<xml><a>home</a></xml> :: <xml><a>search</a></xml> :: [])}
          </div>
          <h1 style={STYLE "text-align: center;"}>{[title]}</h1>
        </div>
        <hr/>
        {bodey}
        <hr/>
        footer
      </body>
    </xml>
    
fun error_page message =
    return <xml>
      <body>
        Error: {[message]}
      </body>
    </xml>

(*
 * internal web pages
 *)

(* TODO: fix this up a bit *)
fun created note title e =
    let
        val content = e.Content
    in
        id <- nextval s;
        dml (INSERT INTO metatbl (
                 Id,
                 Title,
                 Revisions)
             VALUES (
                 {[id]},
                 {[title]},
                 {[0]}));
        t <- Datetime.now;
        dml (INSERT INTO notetbl (
                 Id,
                 Revision,
                 Content,
                 Comment,
                 Time)
             VALUES (
                 {[id]},
                 {[0]},
                 {[content]},
                 {["creation"]},
                 {[Datetime.toTime t]}));
        return <xml>
          <body>
            <h1>Created!</h1>
            <a link={note title}>return</a>
          </body>
        </xml>
    end

fun save note title e =
    let
        val content = e.Content
    in
        (* source of possible failure *)
        m <- oneRow
                 (SELECT metatbl.Id, metatbl.Revisions
                  FROM metatbl
                  WHERE metatbl.Title = {[title]});
        dml (UPDATE metatbl
             SET Revisions = {[m.Metatbl.Revisions+1]}
             WHERE Id = {[m.Metatbl.Id]});
        t <- Datetime.now;
        dml (INSERT INTO notetbl (
                 Id,
                 Revision,
                 Content,
                 Comment,
                 Time)
             VALUES (
                 {[m.Metatbl.Id]},
                 {[m.Metatbl.Revisions+1]},
                 {[content]},
                 {[""]},
                 {[Datetime.toTime t]}));
        return <xml>
          <body>
            <h1>Revised {[title]}!</h1>
            <a link={note title}>go back to it</a>
          </body>
        </xml>
    end
    
    
(*
 * web pages
 *)

fun note title =
    lOpt <- latest_revision title;
    case lOpt of
        None => create title
      | Some l => noteTemplate
                      (<xml><a link={edit title}>edit</a></xml>
                      :: <xml><a link={history title}>history</a></xml>
                      :: [])
                      title
                      <xml>
                        {Markup.markup note l.Content}
                      </xml>
and create title = noteTemplate
                       []
                       ("creating " ^ title)
                       <xml>
                         <form>
                           <textarea{#Content}/>
                           <submit action={created note title}></submit>
                         </form>
                       </xml>
and edit title =
    lOpt <- latest_revision title;
    case lOpt of
        None => create title
      | Some l => noteTemplate
                      (<xml><a link={note title}>revert</a></xml>
                      :: [])
                      ("editing " ^ title)
                      <xml>
                        <form>
                          <textarea{#Content}>{[l.Content]}</textarea>
                          <submit action={save note title}></submit>
                        </form>
                      </xml>
and revision title rev =
    lOpt <- specific_revision title rev;
    case lOpt of
        None => error_page "revision doesn't exist"
      | Some l => noteTemplate
                      (<xml><a link={note title}>current</a></xml>
                      :: <xml><a link={history title}>history</a></xml>
                      :: []) (** TODO BACK, FORWARDS, CURRENT, HISTORY **)
                      (" revision " ^ show rev ^ " of " ^ title)
                      <xml>
                        {[l.Content]}
                        </xml>
and history title =
    let
        fun makeRevision e =
            let
                val t = e.Notetbl.Time
            in
                <xml>[<a link={revision title (e.Notetbl.Revision)}>{[t]}</a>]<br/></xml>
            end
    in
        rOpt <- history_of_revisions title makeRevision;
        case rOpt of
            None => error_page ("There doesn't seem to be a note called " ^ title)
          | Some r => noteTemplate
                          (<xml><a link={note title}>current</a></xml>
                          :: [])
                          ("history of " ^ title)
                          r
    end
