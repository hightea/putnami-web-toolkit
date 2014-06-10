/**
 * This file is part of pwt-code-editor.
 *
 * pwt-code-editor is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-code-editor is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-code-editor.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.code.client.render;

import java.util.List;

import com.google.common.collect.Lists;

import fr.putnami.pwt.plugin.code.client.aspect.CodeEditorAspect;
import fr.putnami.pwt.plugin.code.client.token.Token;
import fr.putnami.pwt.plugin.code.client.token.TokenEvaluator;
import fr.putnami.pwt.plugin.code.client.token.TokenScanner;

public class TextRendererAspect extends AbstractTextRendererAspect {

	private TokenScanner tokenScanner = buildScanner();

	public TextRendererAspect() {
		super();
	}

	public TextRendererAspect(List<TokenEvaluator> evaluators) {
		this();
		this.tokenScanner.registerAllEvaluator(evaluators);
	}

	public TextRendererAspect(boolean autoAddEOLToken, List<TokenEvaluator> evaluators) {
		super(autoAddEOLToken);
		this.tokenScanner.registerAllEvaluator(evaluators);
	}

	@Override
	protected List<Token<?>> extractTokenList(String value) {
		List<Token<?>> tokenizedValue = Lists.newArrayList();
		tokenScanner.setValueToScan(value);
		Token<?> token = tokenScanner.nextToken();
		while (!token.isEOF()) {
			tokenizedValue.add(token);
			token = tokenScanner.nextToken();
		}
		return tokenizedValue;
	}

	public void registerEvaluator(TokenEvaluator evaluator) {
		tokenScanner.registerEvaluator(evaluator);
	}

	@Override
	public CodeEditorAspect copy() {
		return new TextRendererAspect(getAutoAddEOLToken(), tokenScanner.getEvaluators());
	}
}
