/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.code.client.token;

import com.google.common.collect.Lists;

import java.util.List;

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
		this.charScanner.setStringToScan(value);
	}

	@Override
	public void setValueToScan(String value, int startRange, int endRange) {
		this.charScanner.setStringToScan(value, startRange, endRange);
	}

	@Override
	public Token<?> nextToken() {
		this.charScanner.mark();
		int charScanned = this.charScanner.read();
		if (charScanned == CharacterScanner.EOF) {
			return SimpleToken.createEOFToken(this.charScanner.getMark());
		}
		this.charScanner.unread();

		if (this.evaluators != null) {
			for (TokenEvaluator evaluator : this.evaluators) {
				Token<?> token = (evaluator.evaluate(this.charScanner));
				if (!token.isUndefined()) {
					return token;
				}
				this.charScanner.resetToMark();
			}
		}
		// Default
		this.charScanner.read();
		return new SimpleToken<TokenContent>(this.charScanner.getMark(), Character
				.toString((char) charScanned), this.defaultReturnTokenContent);
	}

	@Override
	public void setDefaultReturnTokenContent(TokenContent defaultReturnTokenContent) {
		this.defaultReturnTokenContent = defaultReturnTokenContent;
	}

	@Override
	public List<TokenEvaluator> getEvaluators() {
		return this.evaluators;
	}
}
