package fr.putnami.pwt.core.model.client.exception;

import fr.putnami.pwt.core.model.client.base.EditorModel;

@SuppressWarnings("rawtypes")
public class EditorModelNotInitializedException extends RuntimeException {

	private final EditorModel editorModel;

	public EditorModelNotInitializedException(EditorModel editor) {
		this.editorModel = editor;
	}

	public EditorModel getEditorModel() {
		return editorModel;
	}
}
