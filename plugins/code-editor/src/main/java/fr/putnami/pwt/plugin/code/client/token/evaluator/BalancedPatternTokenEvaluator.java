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
package fr.putnami.pwt.plugin.code.client.token.evaluator;

import com.google.common.base.Preconditions;

import fr.putnami.pwt.plugin.code.client.token.CharacterScanner;
import fr.putnami.pwt.plugin.code.client.token.TokenContent;
import fr.putnami.pwt.plugin.code.client.util.CharacterUtil;

public class BalancedPatternTokenEvaluator extends PatternTokenEvaluator {

	public BalancedPatternTokenEvaluator(String startSequence, String endSequence,
		TokenContent tokenContent, char escapeCharacter, boolean breaksOnEOL, boolean breaksOnEOF) {

		super(startSequence, endSequence, tokenContent, escapeCharacter, breaksOnEOL, breaksOnEOF);
		Preconditions.checkArgument(endSequence != null, "End sequence can not be null.");
	}

	@Override
	protected boolean endSequenceDetected(CharacterScanner scanner, StringBuilder matchingText) {
		int c;
		int openSequence = 1;
		while ((c = scanner.read()) != CharacterScanner.EOF) {
			matchingText.append((char) c);
			if (c == this.escapeCharacter) {
				int nextChar = scanner.read();
				if (nextChar == CharacterScanner.EOF) {
					break;
				}
				matchingText.append((char) nextChar);
			} else if (this.startSequence.length > 0 && c == this.startSequence[0]) {
				if (this.sequenceDetected(scanner, this.startSequence, matchingText, this.breaksOnEOF)) {
					openSequence++;
				}
			} else if (this.endSequence.length > 0 && c == this.endSequence[0]) {
				if (this.sequenceDetected(scanner, this.endSequence, matchingText, this.breaksOnEOF)) {
					if (--openSequence == 0) {
						return true;
					}
				}
			} else if (this.breaksOnEOL) {
				// Check for end of line since it can be used to terminate the pattern.
				for (char[] element : CharacterUtil.END_OF_LINE_DELIMITERS) {
					if (c == element[0]
						&& this.sequenceDetected(scanner, element, matchingText, this.breaksOnEOF)) {
						return true;
					}
				}
			}
		}
		if (this.breaksOnEOF) {
			return true;
		}
		scanner.unread();
		for (int j = 0; j < matchingText.length(); j++) {
			scanner.unread();
		}
		return false;
	}

}
