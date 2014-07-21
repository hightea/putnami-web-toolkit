/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.code.client.token.evaluator;

import java.util.List;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import fr.putnami.pwt.plugin.code.client.token.CharacterScanner;
import fr.putnami.pwt.plugin.code.client.token.SimpleToken;
import fr.putnami.pwt.plugin.code.client.token.Token;
import fr.putnami.pwt.plugin.code.client.token.TokenContent;
import fr.putnami.pwt.plugin.code.client.token.TokenEvaluator;

public class WordsTokenEvaluator implements TokenEvaluator {

	private final WordDetector wordDetector;
	private final TokenContent defaultTokenContent;

	private final List<WordMatcher> wordMatchers = Lists.newArrayList();

	public WordsTokenEvaluator() {
		this(DefaultWordDetector.INSTANCE, null);
	}

	public WordsTokenEvaluator(TokenContent defaultTokenContent) {
		this(DefaultWordDetector.INSTANCE, defaultTokenContent);
	}

	public WordsTokenEvaluator(WordDetector wordDetector, TokenContent defaultTokenContent) {
		this.wordDetector = wordDetector;
		this.defaultTokenContent = defaultTokenContent;
	}

	public void addWordMatcher(WordMatcher wordMatcher) {
		wordMatchers.add(wordMatcher);
	}

	private class WordMatcherSelector implements Predicate<WordMatcher> {

		private String word;

		private WordMatcherSelector(String word) {
			this.word = word;
		}

		@Override
		public boolean apply(WordMatcher input) {
			return input.apply(this.word);
		}
	}

	@Override
	public Token<?> evaluate(CharacterScanner charScanner) {
		int charScanned = charScanner.read();
		if (wordDetector.isWordStart((char) charScanned)) {
			StringBuilder resultText = new StringBuilder();
			do {
				resultText.append((char) charScanned);
				charScanned = charScanner.read();
			}
			while (isWordPart(charScanned));
			charScanner.unread();

			WordMatcher matcher = Iterables.find(wordMatchers, new WordMatcherSelector(resultText.toString()), null);

			TokenContent content = null;
			if (matcher != null) {
				content = matcher.getTokenContent();
			}
			if (content != null || defaultTokenContent != null) {
				content = content != null ? content : defaultTokenContent;
				return new SimpleToken<TokenContent>(charScanner.getMark(), resultText.toString(), content);
			}
			else {
				for (int i = 1; i < resultText.length(); i++) {
					charScanner.unread();
				}
			}
		}
		charScanner.unread();
		return SimpleToken.UNDEFINED;
	}

	private boolean isWordPart(int charScanned) {
		if (charScanned == CharacterScanner.EOF) {
			return false;
		}
		return wordDetector.isWordPart((char) charScanned);
	}

}
