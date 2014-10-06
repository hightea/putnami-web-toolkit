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

import com.google.common.base.CharMatcher;

import fr.putnami.pwt.plugin.code.client.token.CharacterScanner;
import fr.putnami.pwt.plugin.code.client.token.SimpleToken;
import fr.putnami.pwt.plugin.code.client.token.Token;
import fr.putnami.pwt.plugin.code.client.token.TokenEvaluator;

public class WhitespaceTokenEvaluator implements TokenEvaluator {

  @Override
  public Token<?> evaluate(CharacterScanner charScanner) {
    int charScanned = charScanner.read();
    if (matches(CharMatcher.WHITESPACE, charScanned)) {
      StringBuilder resultText = new StringBuilder();
      do {
        resultText.append((char) charScanned);
        charScanned = charScanner.read();
      }
      while (matches(CharMatcher.WHITESPACE, charScanned));
      charScanner.unread();
      return SimpleToken.createWhitespaceToken(charScanner.getMark(), resultText.toString());
    }
    charScanner.unread();
    return SimpleToken.UNDEFINED_TOKEN;
  }

  private boolean matches(CharMatcher charMatcher, int charOrEof) {
    if (charOrEof == CharacterScanner.EOF) {
      return false;
    }

    return charMatcher.matches((char) charOrEof);
  }

}
