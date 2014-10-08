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
package fr.putnami.pwt.plugin.code.client.output;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.SpanElement;
import com.google.gwt.dom.client.Text;
import com.google.gwt.user.client.ui.Widget;

import java.util.List;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.plugin.code.client.render.CssRendererTokenContent;
import fr.putnami.pwt.plugin.code.client.token.Token;

public class CodeLineImpl extends Widget implements CodeLine {

	private static final CssStyle STYLE_LINE = new SimpleStyle("code-editor-line");

	private List<Token<?>> tokenList = Lists.newArrayList();

	public CodeLineImpl() {
		this.setElement(Document.get().createDivElement());
		StyleUtils.addStyle(this, CodeLineImpl.STYLE_LINE);
	}

	@Override
	public void addToken(Token<?> token) {
		this.tokenList.add(token);
	}

	@Override
	public List<Token<?>> getTokens() {
		return this.tokenList;
	}

	@Override
	public void setTokens(List<Token<?>> tokenList) {
		this.tokenList = tokenList;
	}

	@Override
	public void clear() {
		this.tokenList.clear();
		this.redraw();
	}

	@Override
	public void redraw() {
		this.getElement().removeAllChildren();
		for (Token<?> token : this.tokenList) {
			if (token.getContent() != null && token.getContent() instanceof CssRendererTokenContent
				&& ((CssRendererTokenContent) token.getContent()).getCssStyle() != null) {
				SpanElement spanElement = Document.get().createSpanElement();
				spanElement.addClassName(((CssRendererTokenContent) token.getContent()).getCssStyle());
				spanElement.setInnerText(token.getText());
				this.getElement().appendChild(spanElement);
			} else {
				Text textElement = Document.get().createTextNode(token.getText());
				this.getElement().appendChild(textElement);
			}
		}
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof CodeLineImpl) {
			return Objects.equal(this.tokenList, ((CodeLineImpl) other).tokenList);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(this.tokenList);
	}
}
