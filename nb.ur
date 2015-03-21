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
          [
          <a>home</a>
          /
          <a>search</a>
          ]
        </div>
        <h1 style={STYLE "text-align: center;"}>{[title]}</h1>
        </div>
        <hr/>
        {bodey}
        <hr/>
        footer
      </body>
    </xml>

fun makeRevision title revision e =
    let
        val l = revision title (e.Notetbl.Revision)
        val t = e.Notetbl.Time
    in
        <xml>[<a link={l}>{[t]}</a>]<br/></xml>
    end
    
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
                  WHERE metatbl.Title={[title]});
        dml (UPDATE metatbl
             SET Revisions = {[m.Metatbl.Revisions+1]}
             WHERE TRUE);
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
                      (<xml><a link={edit title}>edit</a></xml> :: [])
                      title
                      <xml>
                        {[l.Content]}
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
                      []
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
      | Some l => (* display page *)
        return <xml></xml>
and history title =
    rOpt <- history_of_revisions title (makeRevision title revision);
    case rOpt of
        None =>
        return <xml></xml>
      | Some r =>
        return <xml><body>{r}</body></xml>

(*    
fun make_note f =
    let
        val title = f.Moo
        val content = f.Baz
    in
        id <- nextval s;
        dml (INSERT INTO metatbl (Id, Title, Revisions)
             VALUES ({[id]}, {[title]}, {[0]}));
        t <- Datetime.now;
        dml (INSERT INTO revision (Id, Revision, Content, Time)
             VALUES ({[id]}, {[0]}, {[content]}, {[Datetime.toTime t]}));
        return <xml>
          <body>
            <h1>Created {[title]}!</h1>
            <a link={note title}>go to it</a>
          </body>
        </xml>
    end
        
and revise_note f =
    let
        val title = f.Moo
        val content = f.Baz
    in
        (* source of possible failure *)
        m <- oneRow (SELECT metatbl.Id, metatbl.Revisions
                     FROM metatbl
                     WHERE metatbl.Title={[title]});
        dml (UPDATE metatbl
             SET Revisions = {[m.Metatbl.Revisions+1]}
             WHERE TRUE);
        t <- Datetime.now;
        dml (INSERT INTO revision (Id, Revision, Content, Time)
             VALUES ({[m.Metatbl.Id]}, {[m.Metatbl.Revisions+1]}, {[content]}, {[Datetime.toTime t]}));
        return <xml>
          <body>
            <h1>Revised {[title]}!</h1>
            <a link={note title}>go back to it</a>
          </body>
        </xml>
    end
        
and edit title =
    lOpt <- latest_revision title;
    case lOpt of
        Some l =>
        return <xml>
          <body>
            <h1>Editing {[title]}</h1>
            <form>
              <hidden{#Moo} value={title}></hidden>
              <textarea{#Baz}>{[l.Content]}</textarea>
              <submit action={revise_note}/>
                                          </form>
          </body>
        </xml>
      | None => return <xml></xml>
      
and prepare_note title =
    return <xml>
      <body>
        <h1>{[title]} doesn't exist</h1>
        <p>Do you want to create it?</p>
        <form>
          <hidden{#Moo} value={title}></hidden>
          <textarea{#Baz}></textarea>
          <submit action={make_note} />
        </form>
      </body>
    </xml>
    
and note title =
    lOpt <- latest_revision title;
        case lOpt of
            Some l =>
            return <xml>
              <body>
                <h1>{[title]}</h1> [<a link={edit title}>edit</a>]
                <hr/>
                <pre>
                  {[l.Content]}
                  </pre>
                  <hr/>
              </body>
            </xml>
          | None => prepare_note title

and revisionX title rev =
    lOpt <- specific_revision title rev;
    case lOpt of
        Some l =>
        return <xml>
          <body>
            <h1>{[title]}</h1> [<a link={edit title}>edit</a>]
            <hr/>
            <pre>
              {[l.Content]}
              </pre>
              <hr/>
          </body>
        </xml>
      | None => return <xml></xml> (* todo *)
        
and history title =
    let
        fun makeRevision e = <xml>revision! at [<a link={revisionX title (e.Notetbl.Revision)}>{[e.Notetbl.Time]}</a>]<br/></xml>
    in
        mOpt <- oneOrNoRows (SELECT metatbl.Id, metatbl.Revisions
                             FROM metatbl
                             WHERE metatbl.Title={[title]});
        case mOpt of
            Some m => hist <- queryX (SELECT notetbl.Id, notetbl.Revision, notetbl.Time
                                      FROM revision
                                      WHERE notetbl.Id={[m.Metatbl.Id]})
                                     makeRevision;
            return <xml>
              <body>
                <h1>History of {[title]}</h1>
                {hist}
              </body>
            </xml>
          | None => return <xml></xml> (* TODO *)
    end
                       
fun main () = return <xml>
  <body>
    welcome
  </body>
</xml>
 *)
    
