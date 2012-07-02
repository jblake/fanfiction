drop type site cascade;

drop table stories cascade;
drop table sources cascade;
drop table tags cascade;

drop view all_tags cascade;
drop view story_tags cascade;
drop view unpruned_story_tags cascade;

drop function add_story( ) cascade;
drop function del_story( int ) cascade;
drop function get_filename( int, text ) cascade;
drop function add_source( int, site, text ) cascade;
drop function add_story_source( site, text ) cascade;
drop function add_tag( int, text ) cascade;
drop function del_tag( int, text ) cascade;

begin;

-- This type should be ordered such that better sources are earlier in the list.
create type site as enum
  -- Uncensored sites.
  ( 'fanficauthors.net'
  , 'ficsite.com'
  , 'ficwad.com'
  , 'hpfanficarchive.com'
  , 'patronuscharm.net'
  , 'portkey.org'
  , 'tthfanfic.org'
  , 'yourfanfiction.com'
  -- Censored sites.
  , 'fanfiction.net'
  -- Impossible-to-parse-well sites.
  );

create table stories
  ( story_id serial not null
  , filename text
  , pruned boolean not null default 'f'
  , primary key ( story_id )
  );

create table sources
  ( story_id int not null references stories on delete cascade
  , source site not null
  , ref text not null
  , primary key ( story_id, source )
  );

create table tags
  ( story_id int not null references stories on delete cascade
  , tag text not null
  , primary key ( story_id, tag )
  );

create view all_tags as
  select tag, count(*) as uses
  from tags
  group by tag
  order by uses desc, tag;

create view story_tags as
  select tags.story_id, array_agg(tags.tag) as tags
  from tags
  group by tags.story_id;

create view unpruned_story_tags as
  select story_tags.*
  from story_tags
  inner join stories using (story_id)
  where not pruned;

create function add_story( ) returns int strict volatile as $$
  begin
    insert into stories default values;
    return currval( 'stories_story_id_seq' );
  end;
  $$ language plpgsql;

create function del_story( the_story int ) returns void strict volatile as $$
  begin
    update stories set pruned = 't' where story_id = the_story;
    delete from tags where story_id = the_story;
  end;
  $$ language plpgsql;

create function get_filename( the_story int, the_filename text ) returns text strict volatile as $$
  declare
    old_filename text;
  begin
    select filename into strict old_filename from stories where story_id = the_story;
    if old_filename is null then
      update stories set filename = the_filename where story_id = the_story;
      return the_filename;
    else
      return old_filename;
    end if;
  end;
  $$ language plpgsql;

create function add_source( the_story int, the_source site, the_ref text ) returns void strict volatile as $$
  begin
    delete from sources where story_id = the_story and source = the_source;
    insert into sources ( story_id, source, ref ) values ( the_story, the_source, the_ref );
  end;
  $$ language plpgsql;

create function add_story_source( the_source site, the_ref text ) returns int strict volatile as $$
  declare
    the_story int;
  begin
    select story_id into the_story from sources where source = the_source and ref = the_ref;
    if the_story is null then
      select add_story( ) into the_story;
      perform add_source( the_story, the_source, the_ref );
    end if;
    return the_story;
  end;
  $$ language plpgsql;

create function add_tag( the_story int, the_tag text ) returns void strict volatile as $$
  begin
    if not exists (select * from tags where story_id = the_story and tag = the_tag) then
      insert into tags ( story_id, tag ) values ( the_story, the_tag );
    end if;
  end;
  $$ language plpgsql;

create function del_tag( the_story int, the_tag text ) returns void strict volatile as $$
  begin
    delete from tags where story_id = the_story and tag = the_tag;
  end;
  $$ language plpgsql;

commit;
