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

import com.google.common.base.Preconditions;

import fr.putnami.pwt.plugin.code.client.token.CharacterScanner;
import fr.putnami.pwt.plugin.code.client.token.TokenContent;
import fr.putnami.pwt.plugin.code.client.util.CharacterUtil;

public class BalancedPatternTokenEvaluator extends PatternTokenEvaluator {

	public BalancedPatternTokenEvaluator(String startSequence, String endSequence, TokenContent tokenContent, char escapeCharacter,
			boolean breaksOnEOL, boolean breaksOnEOF) {

		super(startSequence, endSequence, tokenContent, escapeCharacter, breaksOnEOL, breaksOnEOF);
		Preconditions.checkArgument(endSequence != null, "End sequence can not be null.");
	}

	@Override
	protected boolean endSequenceDetected(CharacterScanner scanner, StringBuilder matchingText) {
		int c;
		int openSequence = 1;
		while ((c = scanner.read()) != CharacterScanner.EOF) {
			matchingText.append((char) c);
			if (c == escapeCharacter) {
				int nextChar = scanner.read();
				if (nextChar == CharacterScanner.EOF) {
					break;
				}
				matchingText.append((char) nextChar);
			}
			else if (startSequence.length > 0 && c == startSequence[0]) {
				if (sequenceDetected(scanner, startSequence, matchingText, breaksOnEOF)) {
					openSequence++;
				}
			}
			else if (endSequence.length > 0 && c == endSequence[0]) {
				if (sequenceDetected(scanner, endSequence, matchingText, breaksOnEOF)) {
					if (--openSequence == 0) {
						return true;
					}
				}
			}
			else if (breaksOnEOL) {
				// Check for end of line since it can be used to terminate the pattern.
				for (int i = 0; i < CharacterUtil.END_OF_LINE_DELIMITERS.length; i++) {
					if (c == CharacterUtil.END_OF_LINE_DELIMITERS[i][0]
							&& sequenceDetected(scanner, CharacterUtil.END_OF_LINE_DELIMITERS[i], matchingText, breaksOnEOF)) {
						return true;
					}
				}
			}
		}
		if (breaksOnEOF) {
			return true;
		}
		scanner.unread();
		for (int j = 0; j < matchingText.length(); j++) {
			scanner.unread();
		}
		return false;
	}

}
