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

import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.editor.client.factory.InputFactory;
import fr.putnami.pwt.core.editor.client.factory.OutputFactory;
import fr.putnami.pwt.core.model.client.base.EditorProvider;
import fr.putnami.pwt.core.model.client.base.HasEditorProvider;
import fr.putnami.pwt.core.model.client.base.HasInputEditorFactory;
import fr.putnami.pwt.core.model.client.base.HasOutputEditorFactory;
import fr.putnami.pwt.core.model.client.base.HasWidgetFactory;

public class TableEditorTD<T> extends TableTD<T>
	implements HasOutputEditorFactory<T>, HasInputEditorFactory<T>, HasWidgetFactory,
	HasEditorProvider {

	private OutputFactory outputFactory;
	private InputFactory inputFactory;
	private CloneableWidget widgetFactory;

	private EditorProvider editorProvider;

	public TableEditorTD() {
		super();
	}

	protected TableEditorTD(TableEditorTD<T> source) {
		super(source);
		this.outputFactory = source.outputFactory;
		this.inputFactory = source.inputFactory;
		this.widgetFactory = source.widgetFactory;
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableEditorTD<T>(this);
	}

	public void setInputFactory(InputFactory inputFactory) {
		this.inputFactory = inputFactory;
	}

	@Override
	public InputFactory getInputFactory() {
		return this.inputFactory;
	}

	public void setOutputFactory(OutputFactory outputFactory) {
		this.outputFactory = outputFactory;
	}

	@Override
	public OutputFactory getOutputFactory() {
		return this.outputFactory;
	}

	public void setWidgetFactory(CloneableWidget widgetFactory) {
		this.widgetFactory = widgetFactory;
	}

	@Override
	public CloneableWidget getWidgetFactory() {
		return this.widgetFactory;
	}

	@Override
	public void setEditorProvider(EditorProvider provider) {
		this.editorProvider = provider;
	}

	@Override
	public void redraw() {
		super.redraw();
		this.clear();
		if (this.editorProvider != null) {
			this.append((IsWidget) this.editorProvider.getEditor(this.getReadonly()));
		}
	}

	@Override
	public void add(IsWidget w) {
		throw new UnsupportedOperationException("TableEditorTD does not support add(IsWidget) method");
	}
}
