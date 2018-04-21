def process_one_token(token):
    return dict(is_aplpha=token.is_alpha, is_digit=token.is_digit, is_lower=token.is_lower, is_punct=token.is_punct,
                is_title=token.is_title, pos=token.pos_, lemma=token.lemma_, token_text=token.text)