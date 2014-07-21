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
package fr.putnami.pwt.plugin.code.client.token;

import java.util.List;

import com.google.common.collect.Lists;

public class TokenScannerImpl implements TokenScanner {

	private final CharacterScanner charScanner;

	private List<TokenEvaluator> evaluators = Lists.newArrayList();

	private TokenContent defaultReturnTokenContent;

	public TokenScannerImpl(CharacterScanner charScanner) {
		this.charScanner = charScanner;
	}

	@Override
	public void registerEvaluator(TokenEvaluator evaluator) {
		this.evaluators.add(evaluator);
	}

	@Override
	public void registerAllEvaluator(List<TokenEvaluator> evaluators) {
		if (evaluators != null) {
			this.evaluators.addAll(evaluators);
		}
	}

	@Override
	public void setValueToScan(String value) {
		charScanner.setStringToScan(value);
	}

	@Override
	public void setValueToScan(String value, int startRange, int endRange) {
		charScanner.setStringToScan(value, startRange, endRange);
	}

	@Override
	public Token<?> nextToken() {
		charScanner.mark();
		int charScanned = charScanner.read();
		if (charScanned == CharacterScanner.EOF) {
			return SimpleToken.createEOFToken(charScanner.getMark());
		}
		charScanner.unread();

		if (evaluators != null) {
			for (TokenEvaluator evaluator : evaluators) {
				Token<?> token = (evaluator.evaluate(charScanner));
				if (!token.isUndefined()) {
					return token;
				}
				charScanner.resetToMark();
			}
		}
		// Default
		charScanner.read();
		return new SimpleToken<TokenContent>(charScanner.getMark(), Character.toString((char) charScanned), defaultReturnTokenContent);
	}

	@Override
	public void setDefaultReturnTokenContent(TokenContent defaultReturnTokenContent) {
		this.defaultReturnTokenContent = defaultReturnTokenContent;
	}

	@Override
	public List<TokenEvaluator> getEvaluators() {
		return evaluators;
	}
}
