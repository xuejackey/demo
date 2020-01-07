from typing import List, Dict, Union

import torch
import logging

from collections import Counter
from collections import defaultdict

from segtok.segmenter import split_single
from segtok.tokenizer import split_contractions
from segtok.tokenizer import word_tokenizer


log = logging.getLogger(__name__)


class Sentence1:
    """
    A Sentence1 is a list of Tokens and is used to represent a sentence or text fragment.
    """

    def __init__(self, text: str = None, use_tokenizer: bool = False, labels: Union[List[Label], List[str]] = None):

        super(Sentence1, self).__init__()

        self.tokens: List[Token] = []

        self.labels: List[Label] = []
        if labels is not None: self.add_labels(labels)

        self._embeddings: Dict = {}

        # if text is passed, instantiate sentence with tokens (words)
        if text is not None:

            # tokenize the text first if option selected
            if use_tokenizer:

                # use segtok for tokenization
                tokens = []
                sentences = split_single(text)
                for sentence in sentences:
                    contractions = split_contractions(word_tokenizer(sentence))
                    tokens.extend(contractions)

                # determine offsets for whitespace_after field
                index = text.index
                running_offset = 0
                last_word_offset = -1
                last_token = None
                for word in tokens:
                    try:
                        word_offset = index(word, running_offset)
                        start_position = word_offset
                    except:
                        word_offset = last_word_offset + 1
                        start_position = running_offset + 1 if running_offset > 0 else running_offset

                    token = Token(word, start_position=start_position)
                    self.add_token(token)

                    if word_offset - 1 == last_word_offset and last_token is not None:
                        last_token.whitespace_after = False

                    word_len = len(word)
                    running_offset = word_offset + word_len
                    last_word_offset = running_offset - 1
                    last_token = token

            # otherwise assumes whitespace tokenized text
            else:
                # add each word in tokenized string as Token object to Sentence
                offset = 0
                for word in text.split(' '):
                    if word:
                        try:
                            word_offset = text.index(word, offset)
                        except:
                            word_offset = offset

                        token = Token(word, start_position=word_offset)
                        self.add_token(token)
                        offset += len(word) + 1

    def get_token(self, token_id: int) -> Token:
        for token in self.tokens:
            if token.idx == token_id:
                return token

    def add_token(self, token: Token):
        self.tokens.append(token)

        # set token idx if not set
        token.sentence = self
        if token.idx is None:
            token.idx = len(self.tokens)

    def get_spans(self, tag_type: str, min_score=-1) -> List[Span]:

        spans: List[Span] = []

        current_span = []

        tags = defaultdict(lambda: 0.0)

        previous_tag_value: str = 'O'
        for token in self:

            tag: Label = token.get_tag(tag_type)
            tag_value = tag.value

            # non-set tags are OUT tags
            if tag_value == '' or tag_value == 'O':
                tag_value = 'O-'

            # anything that is not a BIOES tag is a SINGLE tag
            if tag_value[0:2] not in ['B-', 'I-', 'O-', 'E-', 'S-']:
                tag_value = 'S-' + tag_value

            # anything that is not OUT is IN
            in_span = False
            if tag_value[0:2] not in ['O-']:
                in_span = True

            # single and begin tags start a new span
            starts_new_span = False
            if tag_value[0:2] in ['B-', 'S-']:
                starts_new_span = True

            if previous_tag_value[0:2] in ['S-'] and previous_tag_value[2:] != tag_value[2:] and in_span:
                starts_new_span = True

            if (starts_new_span or not in_span) and len(current_span) > 0:
                scores = [t.get_tag(tag_type).score for t in current_span]
                span_score = sum(scores) / len(scores)
                if span_score > min_score:
                    spans.append(Span(
                        current_span,
                        tag=sorted(tags.items(), key=lambda k_v: k_v[1], reverse=True)[0][0],
                        score=span_score)
                    )
                current_span = []
                tags = defaultdict(lambda: 0.0)

            if in_span:
                current_span.append(token)
                weight = 1.1 if starts_new_span else 1.0
                tags[tag_value[2:]] += weight

            # remember previous tag
            previous_tag_value = tag_value

        if len(current_span) > 0:
            scores = [t.get_tag(tag_type).score for t in current_span]
            span_score = sum(scores) / len(scores)
            if span_score > min_score:
                spans.append(Span(
                    current_span,
                    tag=sorted(tags.items(), key=lambda k_v: k_v[1], reverse=True)[0][0],
                    score=span_score)
                )

        return spans

    def add_label(self, label: Union[Label, str]):
        if type(label) is Label:
            self.labels.append(label)

        elif type(label) is str:
            self.labels.append(Label(label))

    def add_labels(self, labels: Union[List[Label], List[str]]):
        for label in labels:
            self.add_label(label)

    def get_label_names(self) -> List[str]:
        return [label.value for label in self.labels]

    @property
    def embedding(self):
        return self.get_embedding()

    def set_embedding(self, name: str, vector):
        self._embeddings[name] = vector.cpu()

    def get_embedding(self) -> torch.autograd.Variable:
        embeddings = []
        for embed in sorted(self._embeddings.keys()):
            embedding = self._embeddings[embed]
            embeddings.append(embedding)

        if embeddings:
            return torch.cat(embeddings, dim=0)

        return torch.FloatTensor()

    def clear_embeddings(self, also_clear_word_embeddings: bool = True):
        self._embeddings: Dict = {}

        if also_clear_word_embeddings:
            for token in self:
                token.clear_embeddings()

    def cpu_embeddings(self):
        for name, vector in self._embeddings.items():
            self._embeddings[name] = vector.cpu()

    def to_tagged_string(self, main_tag=None) -> str:
        list = []
        for token in self.tokens:
            list.append(token.text)

            tags: List[str] = []
            for tag_type in token.tags.keys():

                if main_tag is not None and main_tag != tag_type: continue

                if token.get_tag(tag_type).value == '' or token.get_tag(tag_type).value == 'O': continue
                tags.append(token.get_tag(tag_type).value)
            all_tags = '<' + '/'.join(tags) + '>'
            if all_tags != '<>':
                list.append(all_tags)
        return ' '.join(list)
    def get_tags(self, main_tag=None) -> str:
        list = []
        output = []
        for token in self.tokens:
            list.append(token.text)

            tags: List[str] = []
            for tag_type in token.tags.keys():

                if main_tag is not None and main_tag != tag_type:
                        output.append("O")
                        continue

                #if token.get_tag(tag_type).value == '' or token.get_tag(tag_type).value == 'O': continue
                
                tags.append(token.get_tag(tag_type).value)
            output.append(tags[0])
        return output 
    def to_offset_tags(self, sent_offset):
        list = []
        offset = 0
        cur_entity_start_offset = -1
        cur_entity_text = ""
        for token in self.tokens:
                start = offset
                #for tag_type in token.tags.keys():
                        #if token.get_tag(tag_type) == '' or token.get_tag(tag_type).value == 'O':print (token.text, 'O')
                        #else:  print (token.text, token.get_tag(tag_type).value)	
                offset += len(token.text)
                #print ('-----')
                #print (token.text)
                tag_type = "ner"
                #print ("tag:"+token.get_tag(tag_type).value)
                if token.get_tag(tag_type).value.startswith("E-"):
                        cur_entity_text += token.text
                        entity_start_offset = sent_offset + cur_entity_start_offset
                        entity_end_offset = sent_offset + offset
                        entity_tag = token.get_tag(tag_type).value[2:]
                        entity_txt = cur_entity_text
                        list.append([entity_tag, str(entity_start_offset), str(entity_end_offset), str(entity_txt)])
			#list.append(entity_tag + " " + str(entity_start_offset) + " " + str(entity_end_offset) + "\t"+str(entity_txt))	
                        cur_entity_text = ""
                        cur_entity_start_offset = -1
                if token.get_tag(tag_type).value.startswith("B-"):
                        cur_entity_text = token.text
                        cur_entity_start_offset = offset - len(token.text)
                        if token.whitespace_after:
                                cur_entity_text += " "
                if token.get_tag(tag_type).value.startswith("I-"):
                        cur_entity_text += token.text
                        if token.whitespace_after:
                                cur_entity_text += " "
                if token.get_tag(tag_type).value.startswith("S-"):
                        entity_start_offset = sent_offset + offset - len(token.text)
                        entity_end_offset = sent_offset + offset
                        entity_tag = token.get_tag(tag_type).value[2:]
                        entity_txt = token.text
                        list.append([entity_tag, str(entity_start_offset), str(entity_end_offset), str(entity_txt)]) 
                        #list.append(entity_tag + " " + str(entity_start_offset) + " " + str(entity_end_offset) + "\t"+str(entity_txt))
                        cur_entity_text = ""
                        cur_entity_start_offset = -1		         
                if token.whitespace_after:
                        offset += 1
                end = offset
                #print ("cur_entity:"+cur_entity_text)
        return list		
			


    def to_tokenized_string(self) -> str:
        return ' '.join([t.text for t in self.tokens])

    def to_plain_string(self):
        plain = ''
        for token in self.tokens:
            plain += token.text
            if token.whitespace_after: plain += ' '
        return plain.rstrip()

    def convert_tag_scheme(self, tag_type: str = 'ner', target_scheme: str = 'iob'):

        tags: List[Label] = []
        for token in self.tokens:
            token: Token = token
            tags.append(token.get_tag(tag_type))
        print ("tokens:", self.tokens)
        print ("tags:", tags)
        if target_scheme == 'iob':
            iob2(tags)

        if target_scheme == 'iobes':
            iob2(tags)
            tags = iob_iobes(tags)

        for index, tag in enumerate(tags):
            self.tokens[index].add_tag(tag_type, tag)

    def infer_space_after(self):
        """
        Heuristics in case you wish to infer whitespace_after values for tokenized text. This is useful for some old NLP
        tasks (such as CoNLL-03 and CoNLL-2000) that provide only tokenized data with no info of original whitespacing.
        :return:
        """
        last_token = None
        quote_count: int = 0
        # infer whitespace after field

        for token in self.tokens:
            if token.text == '"':
                quote_count += 1
                if quote_count % 2 != 0:
                    token.whitespace_after = False
                elif last_token is not None:
                    last_token.whitespace_after = False

            if last_token is not None:

                if token.text in ['.', ':', ',', ';', ')', 'n\'t', '!', '?']:
                    last_token.whitespace_after = False

                if token.text.startswith('\''):
                    last_token.whitespace_after = False

            if token.text in ['(']:
                token.whitespace_after = False

            last_token = token
        return self

    def to_original_text(self) -> str:
        str = ''
        pos = 0
        for t in self.tokens:
            while t.start_pos != pos:
                str += ' '
                pos += 1

            str += t.text
            pos += len(t.text)

        return str

    def to_dict(self, tag_type: str = None):
        labels = []
        entities = []

        if tag_type:
            entities = [span.to_dict() for span in self.get_spans(tag_type)]
        if self.labels:
            labels = [l.to_dict() for l in self.labels]

        return {
            'text': self.to_original_text(),
            'labels': labels,
            'entities': entities
        }

    def __getitem__(self, idx: int) -> Token:
        return self.tokens[idx]

    def __iter__(self):
        return iter(self.tokens)

    def __repr__(self):
        return 'Sentence1: "{}" - {} Tokens'.format(' '.join([t.text for t in self.tokens]), len(self))

    def __copy__(self):
        s = Sentence1()
        for token in self.tokens:
            nt = Token(token.text)
            for tag_type in token.tags:
                nt.add_tag(tag_type, token.get_tag(tag_type).value, token.get_tag(tag_type).score)

            s.add_token(nt)
        return s

    def __str__(self) -> str:
        return 'Sentence1: "{}" - {} Tokens'.format(' '.join([t.text for t in self.tokens]), len(self))

    def __len__(self) -> int:
        return len(self.tokens)