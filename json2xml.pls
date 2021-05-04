create or replace function json2xml(
  p_json clob,
  p_root_tag varchar2 default 'root',
  p_item_tag varchar2 default 'item'
) return xmltype as
  type t_tag is record(
    name varchar2(4000),
    type varchar2(6)
  );
  type t_array_tag is table of t_tag;
  
  v_result xmltype;
  v_xml clob := empty_clob();
  v_pos pls_integer := 0;
  v_json_length pls_integer := length(p_json);
  v_char char;
  v_buffer_read varchar2(32767);
  v_buffer_write varchar2(32767) := '<?xml version="1.0"?>';
  v_tag_stack t_array_tag := new t_array_tag();
  v_string varchar2(32767);
  v_is_tag boolean := false;
  v_is_value boolean := false;
  v_skip_read boolean := false;
  v_tag t_tag;
  
  procedure error(p_text varchar2, p_code number default 20404) as
  begin
    raise_application_error(-abs(p_code), p_text);
  end error;
  
  procedure debug(p_text varchar2) as
  begin
    dbms_output.put_line(p_text);
  end debug;
  
  function bool2char(p_bool boolean) return varchar2 as
  begin
    if p_bool then
      return 'True';
    else
      return 'False';
    end if;
    return null;
  end bool2char;
  
  function escape(p_text varchar2) return varchar2 as
    v_result varchar2(32767) := p_text;
  begin
    v_result := replace(v_result, '&', '&amp;');
    v_result := replace(v_result, '<', '&lt;');
    v_result := replace(v_result, '>', '&gt;');
    v_result := replace(v_result, '"', '&quot;');
    return v_result;
  end escape;
  
  function is_numeric(p_text varchar2, p_mask varchar2 default null) return boolean as
    v_num number;
  begin
    if p_mask is null then
      v_num := to_number(p_text);
    else
      v_num := to_number(p_text, p_mask);
    end if;
    return true;
  exception when value_error then
    return false;
  end is_numeric;
  
  function read(p_length pls_integer default 1) return varchar2 as
    v_buffer_pos pls_integer := mod(v_pos, 32767);
    v_amount pls_integer := 32767;
    v_char char;
  begin
    v_pos := v_pos + 1;
    if v_pos <= v_json_length then
      if v_buffer_pos = 0 then
        dbms_lob.read(p_json, v_amount, v_pos, v_buffer_read);
      end if;
      v_char := substr(v_buffer_read, v_buffer_pos + 1, p_length); --TODO: p_length>1
    else
      error('Position (' || v_pos || ') must be less then CLOB length (' || v_json_length || ').');
    end if;
    return v_char;
  end read;
  
  procedure write(p_text varchar2, p_final boolean default false) as
  begin
    --1st var
    /*if lengthb(p_text) > 32767 - lengthb(v_buffer_write) then
      dbms_lob.writeappend(v_xml, length(v_buffer_write), v_buffer_write);
      v_buffer_write := p_text;
    else
      v_buffer_write := v_buffer_write || p_text;
    end if;*/
    --2nd var
    begin
      v_buffer_write := v_buffer_write || p_text;
    exception when value_error then
      dbms_lob.writeappend(v_xml, length(v_buffer_write), v_buffer_write);
      v_buffer_write := p_text;
    end;
    if p_final then
      dbms_lob.writeappend(v_xml, length(v_buffer_write), v_buffer_write);
      v_buffer_write := null;
    end if;
  end write;
  
  function read_string(p_stop_char char default '"', p_length pls_integer default null) return varchar2 as
    v_string varchar2(32767);
    v_unicode varchar2(4);
    v_count pls_integer := 0;
    v_length pls_integer := nvl(p_length, 32767);
  begin
    v_char := read;
    while v_char != p_stop_char and v_count < v_length loop
      case v_char
        when '\' then
          v_char := read;
          case v_char
            when '"' then
              v_string := v_string || v_char;
            when '\' then
              v_string := v_string || v_char;
            when '/' then
              v_string := v_string || v_char;
            when 't' then
              v_string := v_string || chr(9); --tabulator
            when 'n' then
              v_string := v_string || chr(10); --newline
            when 'r' then
              v_string := v_string || chr(12); --formfeed
            when 'f' then
              v_string := v_string || chr(13); --carret
            when 'b' then
              v_string := v_string || chr(8); --backspace
            when 'u' then --unicode
              for i in 1..4 loop
                v_unicode := v_unicode || read;
              end loop;
              if is_numeric(v_unicode, 'xxxx') then
                v_string := v_string || unistr('\' || v_unicode);
              else
                error('Expected hex value but got \u' || v_unicode || '.');
              end if;
              v_unicode := null;
            else
              error('Unexpected ''' || v_char || ''' (' || ascii(v_char) || ') on position ' || v_pos || '.');
          end case;
        else
           v_string := v_string || v_char;
      end case;
      v_count := v_count + 1;
      v_char := read;
    end loop;
    if p_length is not null then
      v_skip_read := true;
    end if;
    return escape(v_string);
  end read_string;
  
  function read_literal(p_is_first_char_ready boolean default true) return varchar2 as
    v_string varchar2(5);
    v_one pls_integer := 0;
  begin
    if p_is_first_char_ready then
      v_string := v_char;
    else
      v_one := 1;
    end if;
    case v_char
      when 't' then
        v_string := v_string || read_string(p_length => 3 + v_one);
        if v_string != 'true' then
          error('Expected true got ' || v_string || ' on position ' || (v_pos - 3 + v_one) || '.');
        end if;
      when 'f' then
        v_string := v_string || read_string(p_length => 4 + v_one);
        if v_string != 'false' then
          error('Expected false got ' || v_string || ' on position ' || (v_pos - 4 + v_one) || '.');
        end if;
      when 'n' then
        v_string := v_string || read_string(p_length => 3 + v_one);
        if v_string != 'null' then
          error('Expected null got ' || v_string || ' on position ' || (v_pos - 3 + v_one) || '.');
        end if;
        v_string := null;
      else
        error('Expected ''t'', ''f'' or ''n'' on position ' || v_pos || '.');
    end case;
    return v_string;
  end read_literal;
  
  function read_number(p_is_first_char_ready boolean default true) return varchar2 as
    v_string varchar2(101);
    v_decimal boolean := false;
    v_minus boolean := false;
  begin
    if not p_is_first_char_ready then 
      v_char := read;
    end if;
    loop
      case v_char
        when '-' then
          if v_string is null and not v_minus then
            v_string := v_char;
            v_minus := true;
          else
            error('Unexpected ''' || v_char || ''' in number in position ' || v_pos || '.');
          end if;
        when '.' then
          if not v_decimal then
            v_string := v_string || v_char;
            v_decimal := true;
          else
            error('Unexpected ''' || v_char || ''' in number in position ' || v_pos || '.');
          end if;
        else
          if v_char in ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9') then
            v_string := v_string || v_char;
          elsif v_char in (',', ']', '}') then
            exit;
          else
            error('Unexpected ''' || v_char || ''' in number in position ' || v_pos || '.');
          end if;
      end case;
      v_char := read;
    end loop;
    v_skip_read := true;
    return v_string;
  end read_number;
  
  procedure open_tag(p_tag varchar2, p_type varchar2 default 'object', p_add boolean default true) as
    v_num pls_integer;
  begin
    begin
      v_num := to_number(substr(p_tag, 1, 1));
      write('<' || p_item_tag || ' id="' || p_tag || '">');
    exception when value_error then
      write('<' || p_tag || '>');
    end;
    if p_add then
      v_tag.name := p_tag;
      v_tag.type := p_type;
      v_tag_stack.extend();
      v_tag_stack(v_tag_stack.last) := v_tag;
    end if;
  end open_tag;
  
  procedure close_tag(p_text clob default null, p_delete boolean default true, p_final boolean default false) as
    v_num pls_integer;
  begin
    if p_text is not null then
      write(p_text);
    end if;
    begin
      v_num := to_number(substr(v_tag_stack(v_tag_stack.last).name, 1, 1));
      write('</' || p_item_tag || '>', p_final);
    exception when value_error then
      write('</' || v_tag_stack(v_tag_stack.last).name || '>', p_final);
    end;
    if p_delete then
      v_tag_stack.delete(v_tag_stack.last);
      if v_tag_stack.count > 0 then
        v_tag := v_tag_stack(v_tag_stack.last);
      else
        v_tag := null;
      end if;
    end if;
  end close_tag;
  
  procedure set_type(p_type varchar2) as
  begin
    v_tag.type := p_type;
    v_tag_stack(v_tag_stack.last).type := p_type;
  end set_type;
  
begin
  dbms_lob.createtemporary(v_xml, true, dbms_lob.session);
  v_char := read;
  case v_char
    when '{' then
      open_tag(p_root_tag);
      v_is_tag := true;
    when '[' then
      open_tag(p_root_tag);
      open_tag(p_item_tag, 'array');
    else
      error('Invalid JSON. Expected ''{'' or ''['' on position ' || v_pos || '.');
  end case;
  while v_pos < v_json_length - 1
  loop
    v_char := read;
    <<char_case>>
    case v_char
      when '{' then
        v_is_tag := true;
      when '}' then
        if v_tag.type = 'object' then
          close_tag;
        elsif v_tag.type = 'array' then
          v_is_tag := false;
        end if;
      when '[' then
        if v_tag.type = 'object' then
          set_type('array');
        elsif v_tag.type = 'array' then
          open_tag(p_item_tag, 'array');
        end if;
      when ']' then
        close_tag(v_string);
        v_string := null;
        v_is_tag := true;
      when ':' then
        open_tag(v_string);
        v_string := null;
        v_is_tag := false;
      when ',' then
        if v_tag.type = 'object' then
          v_is_tag := true;
        elsif not v_is_tag and v_tag.type = 'array' then
          close_tag(v_string, false);
          v_string := null;
          open_tag(v_tag.name, p_add => false);
        end if;
      when '"' then
        v_string := read_string('"');
        v_is_value := true;
      else
        if v_char in ('t', 'f', 'n') then
          v_string := read_literal(true);
          v_is_value := true;
        elsif is_numeric(v_char) or v_char = '-' then
          v_string := read_number(true);
          v_is_value := true;
        elsif v_char in (chr(9), chr(10), chr(13), chr(32)) then
          null;
        else
          error('Unexpected ''' || v_char || ''' (' || ascii(v_char) || ') on position ' || v_pos || '.');
        end if;
    end case;
    if v_is_value then
      if not v_is_tag and v_tag.type = 'object' then
        close_tag(v_string);
        v_string := null;
        v_is_tag := true;
      end if;
      v_is_value := false;
    end if;
    if v_skip_read then
      if v_pos < v_json_length then
        v_skip_read := false;
        goto char_case;
      end if;
    end if;
  end loop;
  if v_skip_read then
    v_skip_read := false;
  else
    v_char := read;
  end if;
  case v_char
    when '}' then
      close_tag(p_final => true);
    when ']' then
      close_tag(v_string);
      v_string := null;
      close_tag(p_final => true);
    else
      error('Invalid JSON. Expected ''}'' or '']'' on position ' || v_pos || '.');
  end case;
  v_result := xmltype(v_xml);
  dbms_lob.freetemporary(v_xml);
  return v_result;
end json2xml;
