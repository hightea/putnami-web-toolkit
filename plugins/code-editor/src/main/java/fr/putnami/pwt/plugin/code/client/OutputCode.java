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
package fr.putnami.pwt.plugin.code.client;

import com.google.gwt.editor.client.LeafValueEditor;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.EditorLeaf;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.helper.TakesValueEditorWrapper;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class OutputCode extends StaticCode implements EditorLeaf, EditorOutput<String> {

	private String path;

	public OutputCode() {
		this.endConstruct();
	}

	protected OutputCode(OutputCode source) {
		super(source);
		this.endConstruct();
	}

	private void endConstruct() {
		StyleUtils.addStyle(this, EditorOutput.STYLE_CONTROL_STATIC);
	}

	@Override
	public IsWidget cloneWidget() {
		return new OutputCode(this);
	}

	@Override
	public void edit(String value) {
		super.edit(value);
	}

	@Override
	public LeafValueEditor<String> asEditor() {
		return new TakesValueEditorWrapper<String>(this);
	}

	@Override
	public String getPath() {
		return this.path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public String getValue() {
		return this.getText();
	}

}
