sequence s

table meta : {
      Id : int,
      Title : string,
      Revisions : int
} PRIMARY KEY Id

table revision : {
      Id : int,
      Revision : int,
      Content : string,
      Time : time
} PRIMARY KEY (Id, Revision)

type result = { Revision : int, Content : string }
fun latest_revision title : transaction (option result) =
    mOpt <- oneOrNoRows (SELECT meta.Id, meta.Revisions
                         FROM meta
                         WHERE meta.Title={[title]});
    case mOpt of
        Some m =>
        prOpt <- oneOrNoRows (SELECT revision.Content
                              FROM revision
                              WHERE revision.Id={[m.Meta.Id]}
                                AND revision.Revision={[m.Meta.Revisions]});
        (case prOpt of
             Some pr => return (Some ({ Revision = m.Meta.Revisions, Content = pr.Revision.Content }))
           | None => return None)
      | None => return None
fun specific_revision title rev =
    mOpt <- oneOrNoRows (SELECT meta.Id
                         FROM meta
                         WHERE meta.Title={[title]});
    case mOpt of
        Some m =>
        prOpt <- oneOrNoRows (SELECT revision.Content
                              FROM revision
                              WHERE revision.Id={[m.Meta.Id]}
                                AND revision.Revision={[rev]});
        (case prOpt of
             Some pr => return (Some ({ Content = pr.Revision.Content }))
           | None => return None)
      | None => return None

                 

fun make_note f =
    let
        val title = f.Moo
        val content = f.Baz
    in
        id <- nextval s;
        dml (INSERT INTO meta (Id, Title, Revisions)
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
        m <- oneRow (SELECT meta.Id, meta.Revisions
                     FROM meta
                     WHERE meta.Title={[title]});
        dml (UPDATE meta
             SET Revisions = {[m.Meta.Revisions+1]}
             WHERE TRUE);
        t <- Datetime.now;
        dml (INSERT INTO revision (Id, Revision, Content, Time)
             VALUES ({[m.Meta.Id]}, {[m.Meta.Revisions+1]}, {[content]}, {[Datetime.toTime t]}));
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
        fun makeRevision e = <xml>revision! at [<a link={revisionX title (e.Revision.Revision)}>{[e.Revision.Time]}</a>]<br/></xml>
    in
        mOpt <- oneOrNoRows (SELECT meta.Id, meta.Revisions
                             FROM meta
                             WHERE meta.Title={[title]});
        case mOpt of
            Some m => hist <- queryX (SELECT revision.Id, revision.Revision, revision.Time
                                      FROM revision
                                      WHERE revision.Id={[m.Meta.Id]})
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
