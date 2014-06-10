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
package fr.putnami.pwt.plugin.code.client.output;

import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.PreElement;
import com.google.gwt.user.client.ui.ComplexPanel;

import fr.putnami.pwt.core.widget.client.base.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class CodeOutputImpl extends ComplexPanel implements CodeOutput {

	private static final CssStyle STYLE_CODE_EDITOR = new SimpleStyle("code-editor");
	private static final CssStyle STYLE_PRE_SCROLLABLE = new SimpleStyle("pre-scrollable");

	private List<CodeLine> lines = Lists.newArrayList();

	private int currentLine;

	public CodeOutputImpl() {
		setElement(Document.get().createElement(PreElement.TAG));
		StyleUtils.addStyle(this, STYLE_CODE_EDITOR);
		StyleUtils.addStyle(this, STYLE_PRE_SCROLLABLE);
	}

	@Override
	public void startRender() {
		currentLine = -1;
	}

	@Override
	public void renderLine(int lineNumber, CodeLine line) {
		currentLine = lineNumber;
		if (lineNumber >= lines.size()) {
			this.lines.add(line);
			line.redraw();
			add(line.asWidget(), getElement());
		}
		else { // Reuse the line
			CodeLine oldLine = this.lines.get(lineNumber);
			if (!oldLine.equals(line)) {
				oldLine.setTokens(line.getTokens());
				oldLine.redraw();
			}
		}
	}

	@Override
	public void renderNextLine(CodeLine line) {
		renderLine(++currentLine, line);
	}

	@Override
	public void endRender() {
		endRender(++currentLine);
	}

	@Override
	public void endRender(int lastLineNumber) {
		// Remove unused lines
		if (this.lines.size() >= lastLineNumber) {
			for (int i = lastLineNumber; i < this.lines.size(); i++) {
				this.lines.get(i).asWidget().removeFromParent();
			}
			this.lines = Lists.newArrayList(this.lines.subList(0, lastLineNumber));
		}
	}

	@Override
	public void clear() {
		for (CodeLine line : lines) {
			line.asWidget().removeFromParent();
		}
		getElement().removeAllChildren();
		this.lines.clear();
	}
}
