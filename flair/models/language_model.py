import torch.nn as nn
import torch
import math
from torch.autograd import Variable
from typing import List
from flair.data import Dictionary


class LanguageModel(nn.Module):
    """Container module with an encoder, a recurrent module, and a decoder."""

    def __init__(self,
                 dictionary: Dictionary,
                 is_forward_lm: bool,
                 hidden_size: int,
                 nlayers: int,
                 embedding_size: int = 100,
                 nout=None,
                 dropout=0.5,
                 best_score=None):

        super(LanguageModel, self).__init__()

        self.dictionary = dictionary
        self.is_forward_lm: bool = is_forward_lm

        self.dropout = dropout
        self.hidden_size = hidden_size
        self.embedding_size = embedding_size
        self.nlayers = nlayers

        self.drop = nn.Dropout(dropout)
        self.encoder = nn.Embedding(len(dictionary), embedding_size)

        if nlayers == 1:
            self.rnn = nn.LSTM(embedding_size, hidden_size, nlayers)
        else:
            self.rnn = nn.LSTM(embedding_size, hidden_size, nlayers, dropout=dropout)

        self.hidden = None

        self.nout = nout
        if nout is not None:
            self.proj = nn.Linear(hidden_size, nout)
            self.initialize(self.proj.weight)
            self.decoder = nn.Linear(nout, len(dictionary))
        else:
            self.proj = None
            self.decoder = nn.Linear(hidden_size, len(dictionary))

        self.init_weights()

        self.best_score = best_score

        # auto-spawn on GPU if available
        if torch.cuda.is_available():
            self.cuda()

    def init_weights(self):
        initrange = 0.1
        self.encoder.weight.data.uniform_(-initrange, initrange)
        self.decoder.bias.data.fill_(0)
        self.decoder.weight.data.uniform_(-initrange, initrange)

    def set_hidden(self, hidden):
        self.hidden = hidden

    def forward(self, input, hidden, ordered_sequence_lengths=None):
        encoded = self.encoder(input)
        emb = self.drop(encoded)

        self.rnn.flatten_parameters()

        output, hidden = self.rnn(emb, hidden)

        if self.proj is not None:
            output = self.proj(output)

        output = self.drop(output)

        decoded = self.decoder(output.view(output.size(0) * output.size(1), output.size(2)))

        return decoded.view(output.size(0), output.size(1), decoded.size(1)), output, hidden

    def init_hidden(self, bsz):
        weight = next(self.parameters()).data
        return (Variable(weight.new(self.nlayers, bsz, self.hidden_size).zero_()),
                Variable(weight.new(self.nlayers, bsz, self.hidden_size).zero_()))

    def get_representation(self, strings: List[str], detach_from_lm=True):

        sequences_as_char_indices: List[List[int]] = []
        for string in strings:
            char_indices = [self.dictionary.get_idx_for_item(char) for char in string]
            sequences_as_char_indices.append(char_indices)

        batch = Variable(torch.LongTensor(sequences_as_char_indices).transpose(0, 1))

        if torch.cuda.is_available():
            batch = batch.cuda()

        hidden = self.init_hidden(len(strings))
        prediction, rnn_output, hidden = self.forward(batch, hidden)

        if detach_from_lm: rnn_output = self.repackage_hidden(rnn_output)

        return rnn_output

    def repackage_hidden(self, h):
        """Wraps hidden states in new Variables, to detach them from their history."""
        if type(h) == torch.Tensor:
            return Variable(h.data)
        else:
            return tuple(self.repackage_hidden(v) for v in h)

    def initialize(self, matrix):
        in_, out_ = matrix.size()
        stdv = math.sqrt(3. / (in_ + out_))
        matrix.data.uniform_(-stdv, stdv)

    @classmethod
    def load_language_model(cls, model_file):

        if not torch.cuda.is_available():
            state = torch.load(model_file, map_location='cpu')
        else:
            state = torch.load(model_file)

        best_score = state['best_score'] if 'best_score' in state else None

        model = LanguageModel(state['dictionary'],
                                             state['is_forward_lm'],
                                             state['hidden_size'],
                                             state['nlayers'],
                                             state['embedding_size'],
                                             state['nout'],
                                             state['dropout'],
                                             best_score)
        model.load_state_dict(state['state_dict'])
        model.eval()
        if torch.cuda.is_available():
            model.cuda()
        return model

    def save(self, file):
        model_state = {
            'state_dict': self.state_dict(),
            'dictionary': self.dictionary,
            'is_forward_lm': self.is_forward_lm,
            'hidden_size': self.hidden_size,
            'nlayers': self.nlayers,
            'embedding_size': self.embedding_size,
            'nout': self.nout,
            'dropout': self.dropout,
            'best_score': self.best_score
        }
        torch.save(model_state, file, pickle_protocol=4)

    def generate_text(self, number_of_characters=1000) -> str:
        with torch.no_grad():
            characters = []

            idx2item = self.dictionary.idx2item

            # initial hidden state
            hidden = self.init_hidden(1)
            input = torch.rand(1, 1).mul(len(idx2item)).long()
            if torch.cuda.is_available():
                input = input.cuda()

            for i in range(number_of_characters):
                prediction, rnn_output, hidden = self.forward(input, hidden)
                word_weights = prediction.squeeze().data.div(1.0).exp().cpu()
                word_idx = torch.multinomial(word_weights, 1)[0]
                input.data.fill_(word_idx)
                word = idx2item[word_idx].decode('UTF-8')
                characters.append(word)

            return ''.join(characters)
