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

import com.google.gwt.user.client.ui.HasHTML;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractComposite;
import fr.putnami.pwt.core.widget.client.util.HTMLUtils;
import fr.putnami.pwt.plugin.code.client.base.CodeEditor;
import fr.putnami.pwt.plugin.code.client.base.CodeEditorDriver;
import fr.putnami.pwt.plugin.code.client.base.CodeEditorDriverImpl;
import fr.putnami.pwt.plugin.code.client.configuration.CodeEditorConfiguration;
import fr.putnami.pwt.plugin.code.client.output.CodeOutput;
import fr.putnami.pwt.plugin.code.client.output.CodeOutputImpl;

public class StaticCode extends AbstractComposite implements CodeEditor, HasHTML {

	private final CodeOutput codeOutput = new CodeOutputImpl();
	private final CodeEditorDriver codeDriver = new CodeEditorDriverImpl(null, this.codeOutput);

	private String value;

	public StaticCode() {
		this.initWidget(this.codeOutput.asWidget());
	}

	protected StaticCode(StaticCode source) {
		super(source);
		this.codeDriver.applyConfiguration(source.codeDriver);
		this.initWidget(this.codeOutput.asWidget());
	}

	@Override
	public IsWidget cloneWidget() {
		return new StaticCode(this);
	}

	public void edit(String value) {
		this.value = value;
		this.codeDriver.edit(value);
	}

	@Override
	public void applyConfiguration(CodeEditorConfiguration configuration) {
		this.codeDriver.applyConfiguration(configuration);
	}

	@Override
	public void setConfiguration(CodeEditorConfiguration configuration) {
		this.codeDriver.setConfiguration(configuration);
	}

	@Override
	public String getText() {
		return this.value;
	}

	@Override
	public void setText(String text) {
		this.edit(text);
	}

	@Override
	public String getHTML() {
		return this.getText();
	}

	/**
	 * To be use only by UIBinder (provide escaped HTML)
	 */
	@Override
	public void setHTML(String html) {
		String htmlToEdit = HTMLUtils.unescapeHTML(html);
		this.setText(htmlToEdit);
	}

}
