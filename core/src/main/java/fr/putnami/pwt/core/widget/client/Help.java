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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.ParagraphElement;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.EditorLabel;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class Help extends AbstractPanel implements EditorLabel {

	private static final CssStyle STYLE_HELP = new SimpleStyle("help-block");

	private String text;

	public Help() {
		super(ParagraphElement.TAG);
		StyleUtils.addStyle(this, Help.STYLE_HELP);
	}

	private Help(Help source) {
		super(source);
		this.setText(source.text);
	}

	@Override
	public String getLabelKey() {
		return null;
	}

	@Override
	public String[] getSuffix() {
		return new String[] {EditorLabel.HELP_SUFFIX};
	}

	@Override
	public boolean isLabelMandatory() {
		return false;
	}

	@Override
	public String getText() {
		return this.text;
	}

	@Override
	public void setText(String message) {
		this.text = message;
		this.getElement().setInnerText(message == null ? "" : message);
	}

	@Override
	public void add(IsWidget child) {
		this.append(child);
	}
}
