       /include jp.rpgleinc

       /*
        * New nodes methods
        */
       dcl-proc jp_newNode;
         dcl-pi *n pointer;
           type int(10) value;
         end-pi;

         dcl-s  pNode pointer;
         dcl-ds node  likeds(jp_node_t) based(pNode);

         pNode = %alloc(%size(jp_node_t));

         node.type = type;
         node.name = '';
         node.value = '';
         node.parent = *NULL;
         node.firstChild = *NULL;
         node.lastChild = *NULL;
         node.nextSibling = *NULL;

         return pNode;
       end-proc;

       dcl-proc jp_newObject export;
         dcl-pi *n pointer;
         end-pi;

         return jp_newNode(JP_TYPE_OBJECT);
       end-proc;

       dcl-proc jp_newArray export;
         dcl-pi *n pointer;
         end-pi;

         return jp_newNode(JP_TYPE_ARRAY);
       end-proc;

       dcl-proc jp_newString export;
         dcl-pi *n pointer;
           str varchar(1024) value;
         end-pi;

         dcl-s  pNode pointer;
         dcl-ds node  likeds(jp_node_t) based(pNode);

         pNode = jp_newNode(JP_TYPE_STRING);
         node.value = str;

         return pNode;
       end-proc;

       dcl-proc jp_newLiteral export;
         dcl-pi *n pointer;
           str varchar(1024) value;
         end-pi;

         dcl-s  pNode pointer;
         dcl-ds node  likeds(jp_node_t) based(pNode);

         pNode = jp_newNode(JP_TYPE_LITERAL);
         node.value = str;

         return pNode;
       end-proc;

       /*
        * Parse JSON
        */
       dcl-proc parse_err;
         dcl-pi *n;
           pSource pointer value;
           ex      varchar(3) value;
         end-pi;

         dcl-s ch     based(pSource);
         dcl-s errmsg varchar(256);

         /* TODO: Make a get error method */

         errmsg = 'Parse Error: Expected ''' + ex + ''''
                + ' instead of ''' + ch + '''.';
       end-proc;

       dcl-proc white;
         dcl-pi *n pointer;
           pSource pointer value;
         end-pi;

         dcl-s ch based(pSource);

         dow %check(ch, JP_WHITE) <> 0;
           pSource++;
         enddo;
       end-proc;

       dcl-proc object;
         dcl-pi *n pointer;
           pSource pointer value;
         end-pi;

         dcl-s ch      based(pSource);
         dcl-s pObject pointer;
         dcl-s key     varchar(1024);
         dcl-s pVal    pointer;

         if ch <> '{';
           parse_err(pSource, '{');
           return *NULL;
         endif;

         pObject = jp_newObject();

         pSource++;
         white(pSource);

         /* Empty object */
         if ch = '}';
           pSource++;
           return pObject;
         endif;

         dow ch <> '';
           key = string(pSource);
           if key = '';
             jp_free(pObject);
             return pObject;
           endif;

           white(pSource);

           if ch <> ':';
             jp_free(pObject);
             parse_err(pSource : ':');
             return *NULL;
           endif;

           pSource++;

           pVal = value(pSource);

           if pVal = *NULL;
             jp_free(pObject);
             return *NULL;
           endif;

           jp_moveInto(pObject : key : pVal);

           white(pSource);

           if ch = '}';
             pSource++;
             return pObject;
           endif;

           if ch <> ',';
             jp_free(object);
             parse_err(pSource, ',');
             return *NULL;
           endif;

           pSource++;
           white(pSource);
         enddo;

         jp_free(pObject);
         parse_err(pSource, '}');

         return *NULL;
       end-proc;

       dcl-proc array;
         dcl-pi *n pointer;
           pSource pointer value;
         end-pi;

         dcl-s ch     based(pSource);
         dcl-s pArray pointer;
         dcl-s pVal   pointer;

         if ch <> '[';
           parse_err(pSource : '[');
           return *NULL;
         endif;

         pSource++;
         pArray = jp_newArray();
         white(pSource);

         /* Empty array */
         if ch = ']';
           pSource++;
           return pArray;
         endif;

         dow ch <> '';
           pVal = value(pSource);
           if pVal = *NULL;
             jp_free(pArray);
             return *NULL;
           endif;

           jp_arrayPush(pArray : pVal);
           white(pSource);

           if ch = ']';
             pSource++;
             return pArray;
           endif;

           if ch <> ',';
             jp_free(array);
             parse_err(pSource : ',');
             return *NULL;
           endif;

           pSource++;
           white(pSource);
         enddo;

         jp_free(pArray);
         parse_err(pSource : ']');
         return *NULL;
       end-proc;

       dcl-proc string;
         dcl-pi *n varchar(1024);
           pSource pointer value;
         end-pi;

         dcl-s ch      based(pSource);
         dcl-s string  varchar(1024);
         dcl-s pString pointer;
         dcl-s new_ch  char(1) based(pString);
         dcl-s len     int(10);

         if ch <> '"';
           parse_err(pSource : '"');
           return *NULL;
         endif;

         pSource++;

         pString = %addr(string) + 2;

         dow ch <> '';
           if ch = '"';
             pSource++;
             %len(string) = len;
             return string;
           elseif ch = '\';
             /* TODO: Handle \uXXXX, \n, \t, \b, ... */
             /* Add backslash */
             new_ch = ch;
             pString++;
             len++;
             pSource++;

             /* Add escaped character */
             new_ch = ch;
             pString++;
             len++;
             pSource++;
           else;
             /* Add character */
             new_ch = ch;
             pString++;
             len++;
             pSource++;
           endif;
         enddo;

         /* Error */
         return '';
       end-proc;

       dcl-proc number;
         dcl-pi *n varchar(256);
           pSource pointer value;
         end-pi;

         dcl-s ch      based(pSource);
         dcl-s string  varchar(1024);
         dcl-s pString pointer;
         dcl-s new_ch  char(1) based(pString);
         dcl-s len     int(10);

         if ch = '-';
           new_ch = ch;
           pString++;
           len++;
           pSource++;
         endif;

         dow %check(ch, '0123456789') <> 0;
           new_ch = ch;
           pString++;
           len++;
           pSource++;
         enddo;

         if ch = '.';
           new_ch = ch;
           pString++;
           len++;
           pSource++;

           dow %check(ch, '0123456789') <> 0;
             new_ch = ch;
             pString++;
             len++;
             pSource++;
           enddo;
         endif;

         if ch = 'e' or ch = 'E';
           new_ch = ch;
           pString++;
           len++;
           pSource++;

           if ch = '-' or ch = '+';
             new_ch = ch;
             pString++;
             len++;
             pSource++;
           endif;

           dow %check(ch, '0123456789') <> 0;
             new_ch = ch;
             pString++;
             len++;
             pSource++;
           enddo;
         endif;

         %len(string) = len;

         return string;
       end-proc;

       dcl-proc literal;
         dcl-pi *n varchar(5);
           pSource pointer value;
         end-pi;

         dcl-s ch based(pSource);
         dcl-s c4 char(4) based(pSource);
         dcl-s c5 char(5) based(pSource);

         if c4 = 'true';
           pSource += 4;
           return 'true';
         endif;

         if c5 = 'false';
           pSource += 5;
           return 'false';
         endif;

         if c4 = 'null';
           pSource += 4;
           return 'null';
         endif;

         return '';
       end-proc;

       dcl-proc value;
         dcl-pi *n pointer;
           pSource  pointer value;
         end-pi;

         dcl-s  ch    based(pSource);
         dcl-s  pNode pointer;
         dcl-ds node  likeds(jp_node) based(pNode);

         white(pSource);

         pNode = *NULL;

         if ch = '{';
           pNode = object(pSource);
         elseif ch = '[';
           pNode = array(pSource);
         elseif ch = '"';
           str = string(pSource);
           if str <> '';
             pNode = jp_newString(str);
           endif;
         elseif %check(ch, '-0123456789') <> 0;
           str = number(pSource);
           /* Numbers are also literals */
           pNode = jp_newLiteral(num);
         else;
            str = literal(pSource);
            if str <> '';
              pNode = jp_newLiteral(str);
            endif;
         endif;

         return pNode;
       end-proc;

       dcl-proc jp_parse export;
         dcl-pi *n pointer;
           source pointer option(*string) value;
         end-pi;

         dcl-s pSource pointer;
         dcl-s pValue  pointer;

         pSource = source + 2;
         pValue = value(pSource);

         white(pSource);

         /* Trailing characters */
         if pValue <> *NULL and ch <> '';
           parse_err(pSource, 'EOF');
           jp_free(pValue);
           return *NULL;
         endif;

         return pValue;
       end-proc;

       /*
        * Stringify JSON
        */
       dcl-proc escape;
         dcl-pi *n varchar(1024);
           string varchar(1024) value;
         end-pi;

         /* TODO: This should reflect string in: jp_parse.rpgle:163 */
         return '"' + string + '"';
       end-proc;

       dcl-proc stringify;
         dcl-pi *n varchar(32000);
           pNode pointer;
         end-pi;

         dcl-ds node likeds(jp_node_t) based(pNode);
         dcl-s ret   varchar(32000);

         if node.type = JP_TYPE_OBJECT;
           ret = '{';
           pNext = node.firstChild;
           dow pNext <> *NULL;
             ret += escape(next.name);
             ret += ':';

             serialize(pNext);

             if pNext.nextSibling;
               ret += ',';
             endif;
             pNext = next.nextSibling;
           enddo;

           ret += '}';
         elseif node.type = JP_TYPE_ARRAY;
           ret = '[';
           pNext = node.firstChild;
           dow pNext <> *NULL;
             serialize(pNext);

             if pNext.nextSibling;
               ret += ',';
             endif;
             pNext = next.nextSibling;
           enddo;

           ret += ']';
         elseif node.type = JP_TYPE_STRING;
           ret += escape(node.value);
         else; /* Literal */
           ret += node.value;
         endif;

         return ret;
       end-proc;

       dcl-proc jp_stringify export;
         dcl-pi *n varchar(32000);
           pNode pointer;
         end-pi;

         return serialize(pNode);
       end-proc;

       /*
        * Utilities
        */
       dcl-proc jp_locate export;
         dcl-pi *n pointer;
           pNode pointer value;
           key   varchar(1024) value;
         end-pi;

         dcl-ds node  likeds(jp_node_t) based(pNode);
         dcl-s  pNext pointer;
         dcl-ds next  likeds(jp_node_t) based(pNext);

         if node.type <> JP_TYPE_OBJECT;
           return 1;
         endif;

         pNext = node.firstChild;

         dow pNext <> *NULL and next.key <> key;
           pNext = next.nextSibling;
         enddo;

         return next;
       end-proc;

       dcl-proc jp_moveInto export;
         dcl-pi *n int(10);
           pNode  pointer value;
           key    varchar(1024) value;
           pChild pointer value;
         end-pi;

         dcl-ds node  likeds(jp_node_t) based(pNode);
         dcl-ds child likeds(jp_node_t) based(pChild);
         dcl-s  pNext pointer;
         dcl-ds next  likeds(jp_node_t) based(pNext);
         dcl-s  pLast pointer;
         dcl-ds last  likeds(jp_node_t) based(pLast);

         if node.type <> JP_TYPE_OBJECT;
           return 1;
         endif;

         jp_detach(pChild);

         child.name = key;
         child.parent = pNode;

         /* Find existing node under `key' */
         pNext = node.firstChild;

         /* pNode have no children */
         if pNext = *NULL;
           node.firstChild = pChild;
           node.lastChild = pChild;
           child.nextSibling = *NULL;
           return 0;
         endif;

         dow pNext <> *NULL;
           if next.name = key;
             leave;
           endif;

           pLast = pNext;
           pNext = next.nextSibling;
         enddo;

         if pNext <> *NULL; /* Found existing child, replace it */
           if pLast <> *NULL;
             last.nextSibling = pChild;
           endif;

           child.nextSibling = next.nextSibling;

           if pNext = node.firstChild;
             node.firstChild = pChild;
           endif;
           if pNext = node.lastChild;
             node.lastChild = pChild;
           endif;

           next.parent = *NULL;
           jp_free(next);
         else; /* Add new value */
           pNext = node.lastChild;
           pNext.nextSibling = pChild;
           node.lastChild = pChild;
           pChild.nextSibling = *NULL;
         endif;

         return 0;
       end-proc;

       dcl-proc jp_arrayPush export;
         dcl-pi *n int(10);
           pNode  pointer value;
           pChild pointer value;
         end-pi;

         dcl-ds node  likeds(jp_node_t) based(pNode);
         dcl-ds child likeds(jp_node_t) based(pChild);
         dcl-s  pLast pointer;
         dcl-ds last  likeds(jp_node_t) based(pLast);

         if node.type <> JP_TYPE_ARRAY;
           return 1;
         endif;

         /* Already in array */
         if child.parent = pNode;
           return 2;
         endif;

         jp_detach(pChild);

         child.parent = pNode;
         child.nextSibling = *NULL;

         if node.lastChild <> *NULL;
           pLast = node.lastChild;
           last.nextSibling = pChild;
           node.lastChild = pChild;
         else;
           node.firstChild = pChild;
           node.lastChild = pChild;
         endif;

         return 0;
       end-proc;

       dcl-proc jp_detach export;
         dcl-pi *n int(10);
           pNode pointer value;
         end-pi;

         dcl-ds node    likeds(jp_node_t) based(pNode);
         dcl-s  pParent pointer;
         dcl-ds parent  likeds(jp_node_t) based(pParent);
         dcl-s  pNext   pointer;
         dcl-ds next    likeds(jp_node_t) based(pNext);
         dcl-s  pLast   pointer;
         dcl-ds last    likeds(jp_node_t) based(pLast);

         pParent = node.parent;

         if pParent = *NULL;
           exsr finish;
         endif;

         pNext = pParent.firstChild;

         if pNode = pParent.firstChild
           parent.firstChild = node.nextSibling;
           if pNode = parent.lastChild;
             parent.lastChild = *NULL;
             exsr finish;
           endif;
         endif;

         dow pNext <> pNode && pNext <> *NULL;
           pLast = pNext;
           pNext = next.nextSibling;
         enddo;

         if pLast <> *NULL;
           last.nextSibling = node.nextSibling;
           if pNode = parent.lastChild;
             parent.lastChild = pLast;
           endif;
         endif;

         exsr finish;

         begsr finish;
           node.name = '';
           node.parent = *NULL;
           node.nextSibling = *NULL;
           return 0;
         endsr;
       end-proc;

       dcl-proc jp_free_;
         dcl-pi *n;
           pNode pointer value;
         end-pi;

         dcl-ds node  likeds(jp_node_t) based(pNode);
         dcl-s  pNext pointer;
         dcl-ds next  likeds(jp_node_t) based(pNext);
         dcl-s  pLast pointer;

         if node.type = JP_TYPE_OBJECT
         or node.type = JP_TYPE_ARRAY;
           pNext = node.firstChild;

           dow pNext <> *NULL;
             pLast = pNext;
             pNext = next.nextSibling;
             jp_free_(pLast);
           enddo;
         endif;

         %dealloc(pNode);

       end-proc;

       dcl-proc jp_free export;
         dcl-pi *n;
           pNode pointer value;
         end-pi;

         jp_detach(pNode);
         jp_free_(pNode);

       end-proc;
